package pl.edu.agh.mock.utils

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.mock.model.{SimulationMap, Tile}
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, Grid, GridPart, Obstacle, WorkerId}

object GridUtils extends LazyLogging{

  def loadDataFromFile(filename: String, grid: Grid)(implicit config: XinukConfig): Unit = {
    val simulationMap: SimulationMap = JsonMapParser.parseMapFromJson(filename)
    val gridArray = simulationMap.getTilesAsArray
    val xOffset = calculateXOffset(grid.workerId, config.workersRoot, config.gridSize - 2)
    val yOffset = calculateYOffset(grid.workerId, config.workersRoot, config.gridSize - 2)
    for (i <- 1 until config.gridSize - 1; j <- 1 until config.gridSize - 1) {
      grid.cells(i)(j) match {
        case EmptyCell.Instance => grid.cells(i)(j) = gridArray(i - 1 + xOffset)(j - 1 + yOffset)
        case _ =>
      }
    }

    updateBufferZone(grid, gridArray, xOffset, yOffset)
  }
  //TODO: Refactor this method
  private def updateBufferZone(grid: Grid, gridArray: Array[Array[GridPart]], xOffset: Int, yOffset: Int)
                              (implicit config: XinukConfig) = {
    // Update left buffer zone
    if (yOffset > 0) {
      for (i <- 1 until config.gridSize - 1) {
        gridArray(xOffset + i - 1)(yOffset - 1) match {
          case Obstacle() => grid.cells(i)(0) = Obstacle()
          case _ =>
        }
      }
    }

    // Update top buffer zone
    if (xOffset > 0) {
      for (i <- 1 until config.gridSize - 1) {
        gridArray(xOffset - 1)(yOffset + i - 1) match {
          case Obstacle() => grid.cells(0)(i) = Obstacle()
          case _ =>
        }
      }
    }

    //Update right buffer zone
    if (yOffset != config.workersRoot - 1) {
      for (i <- 1 until config.gridSize - 1) {
        gridArray(xOffset + i - 1)(yOffset + config.gridSize - 2) match {
          case Obstacle() => grid.cells(i)(config.gridSize - 1) = Obstacle()
          case _ =>
        }
      }
    }

    //Update bottom buffer zone
    if (xOffset != config.workersRoot - 1) {
      for (i <- 1 until config.gridSize - 1) {
        gridArray(xOffset + config.gridSize - 2)(yOffset + i - 1) match {
          case Obstacle() => grid.cells(config.gridSize - 1)(i) = Obstacle()
          case _ =>
        }
      }
    }

    //Update top left corner
    if (xOffset > 0 && yOffset > 0) {
      gridArray(xOffset - 1)(yOffset - 1) match {
        case Obstacle() => grid.cells(0)(0) = Obstacle()
        case _ =>
      }
    }

    //Update top right corner
    if (xOffset > 0 && yOffset != config.workersRoot - 1) {
      gridArray(xOffset - 1)(yOffset + config.gridSize - 2) match {
        case Obstacle() => grid.cells(0)(config.gridSize - 1) = Obstacle()
        case _ =>
      }
    }

    //Update bottom left corner
    if (xOffset != config.workersRoot - 1 && yOffset > 0) {
      gridArray(xOffset + config.gridSize - 2)(yOffset - 1) match {
        case Obstacle() => grid.cells(config.gridSize - 1)(0) = Obstacle()
        case _ =>
      }
    }

    //Update bottom right corner
    if (xOffset != config.workersRoot - 1 && yOffset != config.workersRoot - 1) {
      gridArray(xOffset + config.gridSize - 2)(yOffset + config.gridSize - 2) match {
        case Obstacle() => grid.cells(config.gridSize - 1)(config.gridSize - 1) = Obstacle()
        case _ =>
      }
    }
  }


  private def calculateXOffset(workerId: WorkerId, workersRoot: Int, gridSize: Int): Int = {
    Math.floor((workerId.value - 1) / workersRoot).toInt * gridSize
  }

  private def calculateYOffset(workerId: WorkerId, workersRoot: Int, gridSize: Int): Int = {
    Math.floor((workerId.value - 1) % workersRoot).toInt * gridSize
  }
}

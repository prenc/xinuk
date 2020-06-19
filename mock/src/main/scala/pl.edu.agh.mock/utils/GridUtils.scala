package pl.edu.agh.mock.utils

import com.typesafe.scalalogging.LazyLogging
import org.slf4j.{Logger, LoggerFactory}
import pl.edu.agh.mock.model.{SimulationMap, Tile, TileType}
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, Grid, GridPart, Obstacle, WorkerId}

object GridUtils extends LazyLogging{

  def loadDataFromFile(filename: String, grid: Grid)(implicit config: XinukConfig): Unit = {
    logger.info("Create simulation map for Worker: " +grid.workerId.value)
    val simulationMap: SimulationMap = JsonMapParser.parseMapFromFile(filename)
    val xOffset = calculateXOffset(grid.workerId, config.workersRoot, config.gridSize - 2)
    val yOffset = calculateYOffset(grid.workerId, config.workersRoot, config.gridSize - 2)
    logger.info("Copy values: " +grid.workerId.value)
    copyTileDataToGrid(grid, simulationMap.tiles, xOffset, yOffset)
  }

  private def copyTileDataToGrid(grid: Grid, tiles: List[Tile], xOffset: Int, yOffset: Int) : Unit = {
    tiles.foreach(tile => {
        grid.cells(tile.row)(tile.column) match {
          case Obstacle() => grid.cells(tile.row)(tile.column) = EmptyCell.Instance
          case _ =>
        }
    })
  }

  private def calculateXOffset(workerId: WorkerId, workersRoot: Int, gridSize: Int): Int = {
    Math.floor((workerId.value - 1) / workersRoot).toInt * gridSize
  }

  private def calculateYOffset(workerId: WorkerId, workersRoot: Int, gridSize: Int): Int = {
    Math.floor((workerId.value - 1) % workersRoot).toInt * gridSize
  }
}

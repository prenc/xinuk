package pl.edu.agh.mock.utlis

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model.parallel.MockRoutes
import pl.edu.agh.mock.model.{LocalPoint, MockCell}
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{BufferCell, Grid, InitSmellPropagation, Obstacle, Signal}
import pl.edu.agh.xinuk.utils.Direction

import scala.collection.mutable.ListBuffer

class AlgorithmUtils(val workerId: Int) {
  type DirectionalSmellArray = Array[Array[SmellArray]]

  var directionalSmell: Map[Direction.Value, DirectionalSmellArray] = Map[Direction.Value, DirectionalSmellArray]()

  var transitionsThroughThisWorker: Map[Direction.Value, ListBuffer[Direction.Value]] = Map[Direction.Value, ListBuffer[Direction.Value]]()

  def getTransitionsThroughThisWorker(): Map[Direction.Value, List[Direction.Value]] = {
    var transitions = Map[Direction.Value, List[Direction.Value]]()
    for (direction <- transitionsThroughThisWorker.keys) {
      transitions += (direction -> transitionsThroughThisWorker.apply(direction).toList)
    }
    transitions
  }

  def getDirectionalSmell(): Map[Direction.Value, DirectionalSmellArray] = {
    directionalSmell
  }

  def initializeEmptyListsForTransitions(): Unit = {
    for (direction <- Direction.values) {
      transitionsThroughThisWorker += (direction -> ListBuffer[Direction.Value]())
    }
  }

  def mapTransitionsThroughThisWorker(grid: Grid)(implicit config: MockConfig): Unit = {
    initializeEmptyListsForTransitions()
    for (sourceDirection <- Direction.values) {
      val coordinatesToCheck = coordinatesToCheckFor(sourceDirection)
      for (destinationDirection <- Direction.values) {
        if (sourceDirection != destinationDirection) {
          val destinationDirectionalSmellArray: DirectionalSmellArray = directionalSmell(destinationDirection)
          var sumOfPositive = 0
          for (coordinateToCheck <- coordinatesToCheck) {
            val x: Int = coordinateToCheck._1
            val y: Int = coordinateToCheck._2
            val smellInCoordinateToCheck = destinationDirectionalSmellArray(x)(y)
            val numberOfPossibleDirections = smellInCoordinateToCheck.flatten.count(signal => signal.value != 0)
            if (numberOfPossibleDirections != 0) {
              sumOfPositive += 1
            }
          }
          if (sumOfPositive == coordinatesToCheck.length) {
            transitionsThroughThisWorker.apply(sourceDirection) += destinationDirection
          }
        }
      }
    }
  }

  def coordinatesToCheckFor(direction: Direction.Value)(implicit config: MockConfig): Array[(Int, Int)] = {
    val coordinates = direction match {
      case Direction.TopLeft => Array((1, 1))
      case Direction.Top => Array.range(1, config.gridSize - 1).map(num => (1, num))
      case Direction.TopRight => Array((1, config.gridSize - 2))
      case Direction.Left => Array.range(1, config.gridSize - 1).map(num => (num, 1))
      case Direction.Right => Array.range(1, config.gridSize - 1).map(num => (num, config.gridSize - 2))
      case Direction.BottomLeft => Array((config.gridSize - 2, 1))
      case Direction.Bottom => Array.range(1, config.gridSize - 1).map(num => (config.gridSize - 2, num))
      case Direction.BottomRight => Array((config.gridSize - 2, config.gridSize - 2))
    }
    coordinates.map(coordinate => (coordinate._1 - 1, coordinate._2 - 1))
  }

  def mapLocalDistancesForEveryDirection(grid: Grid)(implicit config: MockConfig): Unit = {
    Direction.values.foreach(direction => {
      mapDistances(grid, direction)
    })

//    Saving directional signal in file for debugging purposes

//    val filename = "signalForWorker" + grid.workerId.value + ".txt"
//    val file = new File(filename)
//    val fw = new FileWriter(file)
//
//    for (direction <- Direction.values) {
//      fw.write("\n")
//      fw.write(direction.toString + "\n")
//      fw.write("\n")
//      for (arrayOfArraysOfSmell <- directionalSmell(direction)) {
//        for (arrayOfSmell <- arrayOfArraysOfSmell) {
//          for (smell <- arrayOfSmell) {
//            smell.map(signal => signal.value).foreach(smellValue => fw.write(smellValue + ", "))
//            fw.write("\n")
//          }
//          fw.write("\n")
//        }
//        fw.write("\n")
//      }
//    }
//
//    fw.close()
  }

  def mapDistances(grid: Grid, direction: Direction.Value)(implicit config: MockConfig): Unit = {
    var newGrid: Grid = Grid.empty(Set(), workerId = grid.workerId)

    for (
      x <- 0 until config.gridSize;
      y <- 0 until config.gridSize
    ) {
      if (grid.cells(x)(y).isInstanceOf[BufferCell]) {
        newGrid.cells(x)(y) = Obstacle()
      } else {
        newGrid.cells(x)(y) = grid.cells(x)(y)
      }

    }

    val coordinates = initialMockCoordinatesFor(direction)

    for (coordinate <- coordinates) {
      if (grid.cells(coordinate._1)(coordinate._2) != Obstacle())
        newGrid.cells(coordinate._1)(coordinate._2) = MockCell.create(Signal(1), List(), LocalPoint(1, 1, grid.workerId), MockRoutes(List[Int](), List[(Int, Int)]()), grid.workerId)
    }

    (0 until (config.gridSize*2)).foreach { _ =>
      val cells = Array.tabulate(config.gridSize, config.gridSize)((x, y) =>
        newGrid.propagatedSignalForInitial(InitSmellPropagation.calculateSmellAddends, x, y)
      )
      newGrid = Grid(cells, newGrid.workerId)
    }

    directionalSmell += (
      direction -> newGrid.cells
        .map(arrayOfGridParts => arrayOfGridParts.map(gridPart => gridPart.smell))
        .drop(1).dropRight(1).map(arrayOfGridParts => arrayOfGridParts.drop(1).dropRight(1))
    )

//  Printing directional signal for debugging purposes

//    for (i <- 0 to math.pow(config.workersRoot, 2).toInt) {
//      if (workerId == i) {
//        println()
//        println(direction.toString())
//        println(workerId)
//        for (arrayOfArraysOfSmell <- directionalSmell(direction)) {
//          for (arrayOfSmell <- arrayOfArraysOfSmell) {
//            for (smell <- arrayOfSmell) {
//              smell.map(signal => signal.value).foreach(smellValue => print(f"$smellValue%5.1f "))
//              println()
//            }
//            println()
//          }
//          println()
//        }
//      }
//      Thread.sleep(300)
//    }
  }

  def initialMockCoordinatesFor(direction: Direction.Value)(implicit config: MockConfig): Array[(Int, Int)] = {
    direction match {
      case Direction.TopLeft => Array((0, 0))
      case Direction.Top => Array.range(1, config.gridSize - 1, (config.gridSize / 10).toInt).map(num => (0, num))
      case Direction.TopRight => Array((0, config.gridSize - 1))
      case Direction.Left => Array.range(1, config.gridSize - 1, (config.gridSize / 10).toInt).map(num => (num, 0))
      case Direction.Right => Array.range(1, config.gridSize - 1, (config.gridSize / 10).toInt).map(num => (num, config.gridSize - 1))
      case Direction.BottomLeft => Array((config.gridSize - 1, 0))
      case Direction.Bottom => Array.range(1, config.gridSize - 1, (config.gridSize / 10).toInt).map(num => (config.gridSize - 1, num))
      case Direction.BottomRight => Array((config.gridSize - 1, config.gridSize - 1))
    }
  }
}
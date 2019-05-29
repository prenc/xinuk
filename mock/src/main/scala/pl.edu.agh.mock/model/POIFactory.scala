package pl.edu.agh.mock.model

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, Grid, WorkerId}

import scala.math.pow
import scala.util.Random


object POIFactory {
  var i = 0
  def generatePOI(grid: Grid)(implicit config : MockConfig): LocalPoint = {
    val random = new Random(System.nanoTime())
    var xDestination: Int = 0
    var yDestination: Int = 0
    var destinationWorkerId: Int = 1

    do {
      xDestination = random.nextInt(config.gridSize - 2) + 1
      yDestination = random.nextInt(config.gridSize - 2) + 1
      destinationWorkerId = random.nextInt(pow(config.workersRoot,2).intValue()) + 1
    } while (
      grid.cells(xDestination)(yDestination) match {
        case EmptyCell(_) => false
        case BufferCell(EmptyCell(_)) => false
        case _ => true
      }
    )

//    if(i % 2 == 0) {
//      xDestination = 12
//      yDestination = 3
//      destinationWorkerId = 1
//
//      i += 1
//    } else {
//      xDestination = 5
//      yDestination = 3
//      destinationWorkerId = 1
//
//      i += 1
//    }
    LocalPoint(xDestination, yDestination, WorkerId(destinationWorkerId))
  }
}

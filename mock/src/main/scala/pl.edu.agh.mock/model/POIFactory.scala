package pl.edu.agh.mock.model

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.xinuk.model.{Grid, WorkerId}

import scala.math.pow
import scala.util.Random


object POIFactory {
  def generatePOI(grid: Grid)(implicit config : MockConfig): LocalPoint = {
    val random = new Random(System.nanoTime())

    val xDestination = random.nextInt(config.gridSize - 2) + 1
    val yDestination = random.nextInt(config.gridSize - 2) + 1
    val destinationWorkerId = random.nextInt(pow(config.workersRoot,2).intValue()) + 1

//    println(xDestination, yDestination, destinationWorkerId)
    LocalPoint(xDestination, yDestination, WorkerId(destinationWorkerId))
  }
}

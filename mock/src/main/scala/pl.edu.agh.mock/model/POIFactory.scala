package pl.edu.agh.mock.model

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.utils.DistanceUtils
import pl.edu.agh.xinuk.model.{Grid, WorkerId}

import scala.math.pow
import scala.util.Random


object POIFactory {
  private final val squareRootOfTwo = math.sqrt(2.0)

  def generatePOI(grid: Grid, x: Int, y: Int, workerId: WorkerId)(implicit config : MockConfig): LocalPoint = {
    val random = new Random(System.nanoTime())

    val xDestination = random.nextInt(config.gridSize - 2) + 1
    val yDestination = random.nextInt(config.gridSize - 2) + 1
    val destinationWorkerId = random.nextInt(pow(config.workersRoot,2).intValue()) + 1

    val distance = DistanceUtils.calculateDistance(LocalPoint(x, y, workerId), LocalPoint(xDestination, yDestination, WorkerId(destinationWorkerId)))

    LocalPoint(xDestination, yDestination, WorkerId(destinationWorkerId), squareRootOfTwo, distance)
  }
}

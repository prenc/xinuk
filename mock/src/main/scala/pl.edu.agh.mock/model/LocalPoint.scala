package pl.edu.agh.mock.model

import pl.edu.agh.xinuk.model.WorkerId

case class LocalPoint(x: Int, y: Int, workerId: WorkerId, var currentDistance: Double = 1.0, distanceInStraightLine: Double = 0.0) {
  private final val squareRootOfTwo = math.sqrt(2.0)

  def addStraightMoveToCurrentDistance(): Unit = {
    currentDistance = currentDistance + 1.0
  }

  def addInclinedMoveToCurrentDistance(): Unit = {
    currentDistance = currentDistance + squareRootOfTwo
  }
}

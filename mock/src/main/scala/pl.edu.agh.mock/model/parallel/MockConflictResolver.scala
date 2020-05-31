package pl.edu.agh.mock.model.parallel

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model.MockCell
import pl.edu.agh.mock.simulation.MockMetrics
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.parallel.ConflictResolver

object MockConflictResolver extends ConflictResolver[MockConfig] {

  var crowdOnSeams = 0

  import Cell._

  override def resolveConflict(current: GridPart, incoming: SmellingCell)(implicit config: MockConfig): (GridPart, MockMetrics) = {
    (current, incoming) match {
      case (Obstacle(), MockCell(_,_,_,_,_,_)) =>
        throw new Exception("Mock encroaches on obstacle from buffer zone")
      case (Obstacle(), _) =>
        (Obstacle(), MockMetrics(0, 0, crowdOnSeams))
      case (EmptyCell(currentSmell), EmptyCell(incomingSmell)) =>
        (EmptyCell(currentSmell + incomingSmell), MockMetrics(0, 0, crowdOnSeams))
      case (MockCell(currentSmell, currentCrowd, destinationPoint, routes, currentWorkerId, currentCovid19), EmptyCell(incomingSmell)) =>
        (MockCell(currentSmell + incomingSmell, currentCrowd, destinationPoint, routes, currentWorkerId, currentCovid19), MockMetrics(0, 0, crowdOnSeams))
      case (EmptyCell(currentSmell), MockCell(incomingSmell, incomingCrowd, destinationPoint, routes, currentWorkerId, currentCovid19)) =>
        (MockCell(currentSmell + incomingSmell, incomingCrowd, destinationPoint, routes, currentWorkerId, currentCovid19), MockMetrics(0, 0, crowdOnSeams))
      case (MockCell(currentSmell, currentCrowd, destinationPoint, routes, currentWorkerId, currentCovid19), incoming@MockCell(incomingSmell, incomingCrowd, _, _, _, incomingCovid19)) =>
        crowdOnSeams += 1
        (MockCell(currentSmell + incomingSmell, currentCrowd ++ List(incoming), destinationPoint, routes, currentWorkerId, currentCovid19 || incomingCovid19), MockMetrics((currentCrowd ++ incomingCrowd).size + 2, 0, crowdOnSeams))
      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}
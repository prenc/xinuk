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
      case (Obstacle(), MockCell(_,_,_,_,_,_,_)) =>
        throw new Exception("Mock encroaches on obstacle from buffer zone")
      case (Obstacle(), _) =>
        (Obstacle(), MockMetrics(0, 0, crowdOnSeams))
      case (EmptyCell(currentSmell), EmptyCell(incomingSmell)) =>
        (EmptyCell(currentSmell + incomingSmell), MockMetrics(0, 0, crowdOnSeams))
      case (MockCell(currentSmell, currentCrowd, destinationPoint, routes, currentWorkerId, currentCovid19, currentTime), EmptyCell(incomingSmell)) =>
        (MockCell(currentSmell + incomingSmell, currentCrowd, destinationPoint, routes, currentWorkerId, currentCovid19, currentTime), MockMetrics(0, 0, crowdOnSeams))
      case (EmptyCell(currentSmell), MockCell(incomingSmell, incomingCrowd, destinationPoint, routes, currentWorkerId, currentCovid19, currentTime)) =>
        (MockCell(currentSmell + incomingSmell, incomingCrowd, destinationPoint, routes, currentWorkerId, currentCovid19, currentTime), MockMetrics(0, 0, crowdOnSeams))
      case (MockCell(currentSmell, currentCrowd, destinationPoint, routes, currentWorkerId, currentCovid19, currentTime), incoming@MockCell(_, _, _, _, _, _, _)) =>
        crowdOnSeams += 1
        (MockCell(currentSmell + incoming.smell, currentCrowd ++ List(incoming), destinationPoint, routes, currentWorkerId, currentCovid19 || incoming.covid19, currentTime), MockMetrics((currentCrowd ++ incoming.crowd).size + 2, 0, crowdOnSeams))
      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}
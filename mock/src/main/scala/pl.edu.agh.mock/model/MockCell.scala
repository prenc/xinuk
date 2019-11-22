package pl.edu.agh.mock.model

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model._


//TODO : Refactor "Mock" name
final case class MockCell(
                           smell: SmellArray,
                           crowd: List[MockCell],
                           var destinationPoint: LocalPoint,
                           var routeThroughWorkers: List[Int],
                           var routeToDestination: List[(Int, Int)],
                           workerId: WorkerId
                         ) extends SmellingCell {

  override type Self = MockCell

  override def withSmell(smell: SmellArray): MockCell = copy(smell = smell)
}

object MockCell {
  def create(
              initialSignal: Signal,
              initialCrowd: List[MockCell] = List(),
              destinationPoint: LocalPoint,
              routeThroughWorkers: List[Int] = List[Int](),
              routeToDestination: List[(Int, Int)] = List[(Int, Int)](),
              workerId: WorkerId
            ): MockCell =
    MockCell(Array.fill(Cell.Size, Cell.Size)(initialSignal), initialCrowd, destinationPoint, routeThroughWorkers, routeToDestination, workerId)
}

trait MockAccessible[+T <: GridPart] {
  def withMock(crowd: List[MockCell], destinationPoint: LocalPoint, routeThroughWorkers: List[Int], routeToDestination: List[(Int, Int)], workerId: WorkerId): T
}

object MockAccessible {
  def unapply(arg: EmptyCell)(implicit config: MockConfig): MockAccessible[MockCell] =
    (crowd: List[MockCell], destinationPoint: LocalPoint, routeThroughWorkers: List[Int], routeToDestination: List[(Int, Int)], workerId: WorkerId) => MockCell(
      arg.smellWith(config.mockInitialSignal),
      crowd,
      destinationPoint,
      routeThroughWorkers,
      routeToDestination,
      workerId
    )

  def unapply(arg: BufferCell)(implicit config: MockConfig): MockAccessible[BufferCell] =
    (crowd: List[MockCell], destinationPoint: LocalPoint, routeThroughWorkers: List[Int], routeToDestination: List[(Int, Int)], workerId: WorkerId) => BufferCell(MockCell(
      arg.smellWith(config.mockInitialSignal),
      crowd,
      destinationPoint,
      routeThroughWorkers,
      routeToDestination,
      workerId
    ))

  def unapply(arg: GridPart)(implicit config: MockConfig): Option[MockAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }
}


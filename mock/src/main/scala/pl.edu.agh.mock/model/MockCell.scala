package pl.edu.agh.mock.model

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model.parallel.MockRoutes
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model._

final case class MockCell(
                           smell: SmellArray,
                           crowd: List[MockCell],
                           var destinationPoint: LocalPoint,
                           var routes: MockRoutes,
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
              routes: MockRoutes = MockRoutes(List[Int](), List[(Int, Int)]()),
              workerId: WorkerId
            ): MockCell =
    MockCell(Array.fill(Cell.Size, Cell.Size)(initialSignal), initialCrowd, destinationPoint, routes, workerId)
}

trait MockAccessible[+T <: GridPart] {
  def withMock(crowd: List[MockCell], destinationPoint: LocalPoint, routes: MockRoutes, workerId: WorkerId): T
}

object MockAccessible {
  def unapply(arg: EmptyCell)(implicit config: MockConfig): MockAccessible[MockCell] =
    new MockAccessible[MockCell] {
      override def withMock(
                             crowd: List[MockCell],
                             destinationPoint: LocalPoint,
                             routes: MockRoutes,
                             workerId: WorkerId
                           ): MockCell =
        MockCell(
          arg.smellWith(config.mockInitialSignal),
          crowd,
          destinationPoint,
          routes,
          workerId
        )
    }

  def unapply(arg: BufferCell)(implicit config: MockConfig): MockAccessible[BufferCell] =
    new MockAccessible[BufferCell] {
      override def withMock(
                             crowd: List[MockCell],
                             destinationPoint: LocalPoint,
                             routes: MockRoutes,
                             workerId: WorkerId
                           ): BufferCell =
        BufferCell(MockCell(
          arg.smellWith(config.mockInitialSignal),
          crowd,
          destinationPoint,
          routes,
          workerId
        ))
    }

  def unapply(arg: GridPart)(implicit config: MockConfig): Option[MockAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }
}


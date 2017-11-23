package pl.edu.agh.formin.model

import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, GridPart, SmellingCell}


final case class AlgaeCell(smell: SmellArray, lifespan: Long) extends SmellingCell {
  override type Self = AlgaeCell

  override def withSmell(smell: SmellArray): AlgaeCell = copy(smell = smell)
}

trait AlgaeAccessible[+T] {
  def withAlgae(lifespan: Long): T
}
object AlgaeAccessible {

  def unapply(arg: EmptyCell)(implicit config: ForminConfig): AlgaeAccessible[AlgaeCell] =
    lifespan => AlgaeCell(arg.smellWith(config.algaeInitialSignal), lifespan)

  def unapply(arg: BufferCell)(implicit config: ForminConfig): AlgaeAccessible[BufferCell] =
    lifespan => BufferCell(AlgaeCell(arg.smellWith(config.algaeInitialSignal), lifespan))

  def unapply(arg: GridPart)(implicit config: ForminConfig): Option[AlgaeAccessible[GridPart]] = arg match {
    case cell@EmptyCell(_) => Some(unapply(cell))
    case cell@BufferCell(_) => Some(unapply(cell))
    case _ => None
  }
}

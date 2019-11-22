package pl.edu.agh.xinuk.utils

object Direction extends Enumeration {
  val TopLeft, Top, TopRight, Left, Right, BottomLeft, Bottom, BottomRight = Value

  def reversed(direction: Direction.Value): Direction.Value = {
    direction match {
      case TopLeft => BottomRight
      case Top => Bottom
      case TopRight => BottomLeft
      case Left => Right
      case Right => Left
      case BottomLeft => TopRight
      case Bottom => Top
      case BottomRight => TopLeft
    }
  }
}
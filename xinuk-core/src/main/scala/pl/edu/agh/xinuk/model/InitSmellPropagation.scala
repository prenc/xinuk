package pl.edu.agh.xinuk.model

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.Grid.{CellArray, SubcellCoordinates}

object InitSmellPropagation {
  private final val squareRootOfTwo = math.sqrt(2.0)

  def calculateSmellAddends(cells: CellArray, x: Int, y: Int)(implicit config: XinukConfig): Vector[Option[Signal]] = {
    @inline def destinationCellSignal(i: Int, j: Int): Option[SmellArray] = {
      cells.lift(x + i - 1).flatMap(_.lift(y + j - 1).map(_.smell))
    }

    SubcellCoordinates.map {
      case (i, j) if i == 1 || j == 1 =>
        destinationCellSignal(i, j).map(signal => {
          val straight = Signal(
            signal(i)(j).value
          )
          val firstCurve = Signal(
            (signal(i + j - 1)(i + j - 1).value + signal(i - j + 1)(j - i + 1).value) / math.pow(config.gridSize, 2)
          )
          val secondCurve = Signal(
            (signal(j)(i).value + signal(math.abs(j-2))(math.abs(i-2)).value) / math.pow(config.gridSize, 3)
          )
          straight + firstCurve + secondCurve
        })
      case (i, j) =>
        destinationCellSignal(i, j).map(signal => {
          val straight = Signal(
            signal(i)(j).value / squareRootOfTwo
          )
          val firstCurve = Signal(
            (signal(i / 2 - j / 2 + 1)(j / 2 + i / 2).value + signal(i / 2 + j / 2)(j /2 - i / 2 + 1).value) / (math.pow(config.gridSize, 2) * squareRootOfTwo)
          )
          val secondCurve = Signal(
            (signal(i)(math.abs(j-2)).value + signal(math.abs(i-2))(j).value) / (math.pow(config.gridSize, 3) * squareRootOfTwo)
          )
          straight + firstCurve + secondCurve
        })
    }
  }

  def outOfBounds(a: Int, b: Int, cells: CellArray): Boolean = {
    a >= cells.length || b >= cells(0).length || a < 0 || b < 0
  }

}

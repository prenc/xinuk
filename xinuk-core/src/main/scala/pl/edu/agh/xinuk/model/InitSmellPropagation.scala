package pl.edu.agh.xinuk.model

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.Grid.{CellArray, SubcellCoordinates}

object InitSmellPropagation {

  def calculateSmellAddends(cells: CellArray, x: Int, y: Int)(implicit config: XinukConfig): Vector[Option[Signal]] = {
    @inline def destinationCellSignal(i: Int, j: Int): Option[SmellArray] = {
      cells.lift(x + i - 1).flatMap(_.lift(y + j - 1).map(_.smell))
    }

    SubcellCoordinates.map {
      case (i, j) if i == 1 || j == 1 =>
        destinationCellSignal(i, j).map(signal =>
          signal(i)(j) + Signal((signal(i + j - 1)(i + j - 1).value + signal(i - j + 1)(j - i + 1).value)/(config.gridSize*config.gridSize))
        )
      case (i, j) =>
        destinationCellSignal(i, j).map(signal => {
          signal(i)(j) + Signal((signal(i / 2 - j / 2 + 1)(j / 2 + i / 2).value + signal(i / 2 + j / 2)(j /2 - i / 2 + 1).value)/(config.gridSize*1.41421*config.gridSize))
        })
    }
  }

  def outOfBounds(a: Int, b: Int, cells: CellArray): Boolean = {
    a >= cells.length || b >= cells(0).length || a < 0 || b < 0
  }

}

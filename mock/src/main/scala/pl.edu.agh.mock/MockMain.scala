package pl.edu.agh.mock

import java.awt.Color
import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.mock.algorithm.MockMovesController
import pl.edu.agh.mock.model.MockCell
import pl.edu.agh.mock.model.parallel.MockConflictResolver
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.{DefaultSmellPropagation, Obstacle, SmellingCell}

object MockMain extends LazyLogging {
  private val configPrefix = "mock"
  private val metricHeaders = Vector()

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation(
      configPrefix,
      metricHeaders,
      MockConflictResolver,
      DefaultSmellPropagation.calculateSmellAddends)(new MockMovesController(_)(_),
      {
        case MockCell(_, _, _, _, _, x, y) =>
          if (x && y >= 0 && y < 200) {
            Color.yellow
          } else if (x && y >= 200 && y < 450) {
            Color.orange
          } else if (x && y >= 450 && y < 600) {
            Color.red
          } else if (!x && y >= 600) {
            Color.green
          } else if (x && y >= 600) {
            Color.blue
          } else {
            Color.white
          }
        case Obstacle() => Color.black
        case cell: SmellingCell => cellToColorRegions(cell)
      }).start()
  }

  private def cellToColorRegions(cell: SmellingCell): Color = {
    val smellValue = cell.smell.map(_.map(_.value).sum).sum.toFloat
    val brightness = 1.toFloat - Math.pow(smellValue, 0.1).toFloat
    val saturation = 0f
    val hue = 0f
    Color.getHSBColor(hue, saturation, brightness)
  }

}


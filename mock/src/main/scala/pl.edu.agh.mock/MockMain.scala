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

  val INCUBATION_PERIOD = 775 // 5-6 days
  val CONVALESCENT = 1800 // 10-14 days

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation(
      configPrefix,
      metricHeaders,
      MockConflictResolver,
      DefaultSmellPropagation.calculateSmellAddends)(new MockMovesController(_)(_),
      {
        case MockCell(_, _, _, _, _, x, y) =>
          if (x && y >= 0 && y < INCUBATION_PERIOD) {
            Color.orange
          } else if (x && y >= INCUBATION_PERIOD && y < CONVALESCENT) {
            Color.red
          } else if (!x && y >= CONVALESCENT) {
            Color.green
          } else {
            Color.pink
          }
        case Obstacle() => Color.blue
        //case cell: SmellingCell => cellToColorRegions(cell)
      }).start()
  }

  private def cellToColorRegions(cell: SmellingCell): Color = {
    val smellValue = cell.smell.map(_.map(_.value).sum).sum.toFloat
    val brightness = 1.toFloat - Math.pow(smellValue, 0.1).toFloat
    val saturation = 0f
    val hue = 0f
    Color.getHSBColor(hue, saturation, brightness)
    Color.white
  }

}


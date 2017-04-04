package pl.edu.agh.formin.gui

import java.awt.image.BufferedImage
import java.awt.{Color, Dimension}
import javax.swing.{BorderFactory, ImageIcon, UIManager}

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.jfree.chart.{ChartFactory, ChartPanel}
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import pl.edu.agh.formin.SchedulerActor.{IterationFinished, Register}
import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.formin.model._
import pl.edu.agh.formin.{IterationStatus, WorkerId}

import scala.swing.BorderPanel.Position._
import scala.swing.TabbedPane.Page
import scala.swing.Table.AbstractRenderer
import scala.swing._
import scala.swing.event.ButtonClicked
import scala.util.Try

class GuiActor private(scheduler: ActorRef, worker: WorkerId, guiType: GuiType)(implicit config: ForminConfig)
  extends Actor with ActorLogging {

  import GuiActor._

  override def receive: Receive = started

  private lazy val gui: GuiGrid = new GuiGrid(config.gridSize, guiType)(iteration =>
    scheduler ! IterationFinished(iteration)
  )

  override def preStart: Unit = {
    scheduler ! Register
    log.info("GUI started")
  }

  def started: Receive = {
    case NewIteration(state, iteration) =>
      state.getGridForWorker(worker) match {
        case Some(grid) =>
          gui.setNewValues(grid, iteration)
        case None =>
          log.error("Worker {} grid status unavailable", worker.value)
      }
  }
}

object GuiActor {

  case class NewIteration(state: IterationStatus, iteration: Long)

  def props(scheduler: ActorRef, worker: WorkerId, guiType: GuiType)(implicit config: ForminConfig): Props = {
    Props(new GuiActor(scheduler, worker, guiType))
  }
}

private[gui] class GuiGrid(dimension: Int, guiType: GuiType)(onNextIterationClicked: Long => Unit) extends SimpleSwingApplication {

  Try(UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName))

  private val bgcolor = new Color(220, 220, 220)
  private var xs = Seq.empty[Double]
  private var ys1 = Seq.empty[Long]
  private var ys2 = Seq.empty[Long]
  private var startTime = System.currentTimeMillis()
  private val k = false
  private val tb = guiType match {
    case GuiType.None => new ParticleCanvas(0)
    case GuiType.Basic => new ParticleCanvas(dimension)
    case GuiType.Signal => new SignalTable(dimension)
  }

  private val chartPanel = new BorderPanel {
    background = bgcolor
  }
  private val chartPage = new Page("Plot", chartPanel)
  private val iterationLabel = new Label {
    private var _iteration: Long = _

    def iteration: Long = _iteration

    def setIteration(iteration: Long): Unit = {
      _iteration = iteration
      text = s"Iteration: $iteration"
    }

    border = BorderFactory.createEmptyBorder(50, 20, 50, 20)
  }
  private val nextIterationButton = new Button("Next iteration") {
    border = BorderFactory.createEmptyBorder(100, 20, 20, 20)
  }
  listenTo(nextIterationButton)
  reactions += {
    case ButtonClicked(`nextIterationButton`) =>
      nextIterationButton.enabled = false
      onNextIterationClicked(iterationLabel.iteration)
  }

  def top = new MainFrame {
    title = "Formin model"
    minimumSize = new Dimension(1200, 800)
    background = bgcolor

    val mainPanel = new BorderPanel {

      val signalPanel = new BorderPanel {
        val table = new BorderPanel {
          background = bgcolor
          layout(tb) = Center
        }
        background = bgcolor
        layout(table) = Center
      }


      val contentPane = new TabbedPane {
        pages += new Page("Signal", signalPanel)
        pages += chartPage
      }

      val statusPanel = new BorderPanel {
        layout(iterationLabel) = North
        layout(nextIterationButton) = Center
      }

      layout(contentPane) = Center
      layout(statusPanel) = East
    }

    contents = mainPanel
  }

  def setNewValues(newGrid: Grid, iteration: Long): Unit = {
    tb.set(newGrid.cells)
    tb.repaint()
    updateForminAlgaeCount(newGrid.cells)
    plot()
    iterationLabel.setIteration(iteration)
    nextIterationButton.enabled = true
  }

  def updateForminAlgaeCount(cells: Array[Array[Cell]]): Unit = {
    var forminCounter = 0
    var algaeCounter = 0

    for {
      x <- cells.indices
      y <- cells.indices
    } {
      cells(x)(y) match {
        case AlgaeCell(_) => algaeCounter = algaeCounter + 1
        case ForaminiferaCell(_, _) => forminCounter = forminCounter + 1
        case _ =>
      }
    }
    xs = xs :+ (System.currentTimeMillis() - startTime) / 1000d
    ys1 = ys1 :+ forminCounter.toLong
    ys2 = ys2 :+ algaeCounter.toLong

  }

  sealed trait VisualizationSetter {
    def set(cells: Array[Array[Cell]]): Unit
  }

  private class SignalTable(dimension: Int) extends Table(3 * dimension, 3 * dimension) with VisualizationSetter {
    private val algaeColor = new swing.Color(9, 108, 16)
    private val forminColor = new swing.Color(81, 71, 8)
    private val obstacleColor = new swing.Color(0, 0, 0)
    private val emptyColor = new swing.Color(255, 255, 255)
    private val renderer = new AbstractRenderer(new CellLabel) {
      override def configure(table: Table, isSelected: Boolean, hasFocus: Boolean, a: Any, row: Int, column: Int): Unit = {
        component.prepare(row, column)
      }
    }

    var cells: Array[Array[Cell]] = _

    class CellLabel extends Label {

      def prepare(row: Int, column: Int) {
        text = cells(column / 3)(row / 3).smell(column % 3)(row % 3).value.toString
        background = cells(column / 3)(row / 3) match {
          case AlgaeCell(_) => algaeColor
          case ForaminiferaCell(energy, _) => if (row % 3 == 1 && column % 3 == 1) {
            text = energy.value.toString
            new swing.Color(255, (0 + 255 * energy.value / 2).toInt, (0 + 255 * energy.value / 2).toInt)
          } else {
            forminColor
          }
          case Obstacle => obstacleColor
          case EmptyCell(_) => emptyColor
        }


      }
    }

    def set(cells: Array[Array[Cell]]): Unit = {
      this.cells = cells
      this.repaint()
    }

    override def rendererComponent(sel: Boolean, foc: Boolean, row: Int, col: Int): Component = {
      renderer.componentFor(this, isSelected = false, hasFocus = false, 0, row, col)
    }

  }

  private class ParticleCanvas(dimension: Int) extends Label with VisualizationSetter {
    private val factor = 5
    private val algaeColor = new swing.Color(9, 108, 16).getRGB
    private val forminColor = new swing.Color(81, 71, 8).getRGB
    private val obstacleColor = new swing.Color(0, 0, 0).getRGB
    private val emptyColor = new swing.Color(255, 255, 255).getRGB
    private val img = new BufferedImage(dimension * factor, dimension * factor, BufferedImage.TYPE_INT_ARGB)

    icon = new ImageIcon(img)

    def set(cells: Array[Array[Cell]]): Unit = {
      val rgbArray = cells.map(_.map {
        case AlgaeCell(_) => algaeColor
        case ForaminiferaCell(_, _) => forminColor
        case Obstacle => obstacleColor
        case EmptyCell(_) => emptyColor
      })

      for {
        x <- cells.indices
        y <- cells.indices
      } {
        val startX = x * factor
        val startY = y * factor
        img.setRGB(startX, startY, factor, factor, Array.fill(factor * factor)(rgbArray(x)(y)), 0, factor)
      }
    }

  }

  def plot(): Unit = {
    val dataset = new XYSeriesCollection()
    val s1 = new XYSeries("Algae")
    val s2 = new XYSeries("Formin")
    for (i <- xs.indices) {
      s1.add(xs(i), ys1(i))
      s2.add(xs(i), ys2(i))
    }
    dataset.addSeries(s1)
    dataset.addSeries(s2)
    val chart = ChartFactory.createXYLineChart("Formin vs Algae occurences in time", "Time[s]", "Number of occurrences", dataset, PlotOrientation.VERTICAL, true, true, false)
    val panel = new ChartPanel(chart)
    chartPanel.layout(swing.Component.wrap(panel)) = Center
  }

  trait Evaluable {
    def value: Double
  }

  main(Array.empty)

}

sealed trait GuiType

object GuiType {

  case object None extends GuiType

  case object Basic extends GuiType

  case object Signal extends GuiType

}



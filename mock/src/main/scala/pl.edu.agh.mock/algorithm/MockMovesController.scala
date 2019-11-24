package pl.edu.agh.mock.algorithm

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model._
import pl.edu.agh.mock.model.parallel.MockRoutes
import pl.edu.agh.mock.simulation.MockMetrics
import pl.edu.agh.mock.utils.{GlobalAStarAlgorithmUtils, GridUtils}
import pl.edu.agh.mock.utlis.{AlgorithmUtils, LocalAStarAlgorithmUtils}
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Obstacle, _}
import pl.edu.agh.xinuk.utils.Direction
import scala.collection.immutable.TreeSet

final class MockMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: MockConfig) extends MovesController {
  var crowdOnProcessor = 0
  var transitionsThroughWorkers: Map[Int, Map[Direction.Value, List[Direction.Value]]] = Map[Int, Map[Direction.Value, List[Direction.Value]]]()
  var algorithmUtils = new AlgorithmUtils(0)
  var receivedMessages = 0
  var workerId = 0
  var directionalSmell: Map[Direction.Value, Array[Array[SmellArray]]] = Map[Direction.Value, Array[Array[SmellArray]]]()

  override def receiveMessage(message: Any): Unit = {
    val tuple = message.asInstanceOf[(Int, Map[Direction.Value, List[Direction.Value]])]
    transitionsThroughWorkers += (tuple._1 -> tuple._2)
    receivedMessages += 1
  }

  override def initialGrid(workerId: WorkerId): (Grid, MockMetrics, Map[Direction.Value, List[Direction.Value]]) = {
    this.workerId = workerId.value
    algorithmUtils = new AlgorithmUtils(this.workerId)

    val grid = Grid.empty(bufferZone, workerId = workerId)

//    GridUtils.loadDataFromFile("map.json", grid)
//    if (workerId.value == 1) {
//      grid.cells(1)(5) = Obstacle
//      grid.cells(2)(5) = Obstacle
//      grid.cells(3)(5) = Obstacle
//      grid.cells(4)(5) = Obstacle
//      grid.cells(5)(5) = Obstacle
//      grid.cells(6)(5) = Obstacle
//      grid.cells(7)(5) = Obstacle
//      grid.cells(8)(5) = Obstacle
//      grid.cells(9)(5) = Obstacle
//
//      grid.cells(14)(11) = Obstacle
////      grid.cells(13)(11) = Obstacle
//      grid.cells(12)(11) = Obstacle
//      grid.cells(11)(11) = Obstacle
//      grid.cells(10)(11) = Obstacle
//      grid.cells(9)(11) = Obstacle
//      grid.cells(8)(11) = Obstacle
//      grid.cells(7)(11) = Obstacle
//      grid.cells(6)(11) = Obstacle
//    }

    algorithmUtils.mapLocalDistancesForEveryDirection(grid)
    directionalSmell = algorithmUtils.getDirectionalSmell()

    algorithmUtils.mapTransitionsThroughThisWorker(grid)

    if (grid.cells(config.gridSize / 2)(config.gridSize / 2).isInstanceOf[EmptyCell] && workerId.value == 2) {
      val mock = MockCell.create(
        config.mockInitialSignal,
        destinationPoint = POIFactory.generatePOI(grid),
        workerId = grid.workerId
      )
      grid.cells(config.gridSize / 2)(config.gridSize / 2) = mock
    }

    val metrics = MockMetrics.empty()
    println("Grid initialized")
    (grid, metrics, algorithmUtils.getTransitionsThroughThisWorker())
  }

  def setGlobalRoute(mock: MockCell, workerId: Int, destinationWorker: Int, x: Int, y: Int): Unit = {
    mock.routes.routeThroughWorkers = GlobalAStarAlgorithmUtils.aStar(
      workerId,
      destinationWorker,
      x,
      y,
      directionalSmell,
      transitionsThroughWorkers
    )
  }

  def setLocalRoute(mock: MockCell, x: Int, y: Int, grid: Grid): Unit = {
    mock.routes.routeToDestination = LocalAStarAlgorithmUtils.aStar(
      (x, y),
      (mock.destinationPoint.x, mock.destinationPoint.y),
      grid
    )
  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, MockMetrics) = {

    val newGrid = Grid.empty(bufferZone, workerId = grid.workerId)
    Thread.sleep(300)

    def copyCells(x: Int, y: Int, cell: GridPart): Unit = {
      newGrid.cells(x)(y) = cell
    }

    def moveCells(x: Int, y: Int, cell: GridPart): Unit = {

      def makeMockMove(occupiedCell: MockCell): Unit = {
        var point = (0, 0)
        if (x != 0 && y != 0 && x != config.gridSize && y != config.gridSize)
          while (occupiedCell.destinationPoint == LocalPoint(x, y, WorkerId(workerId)) || !isDestinationPointAccessible(grid, occupiedCell)) {
            occupiedCell.destinationPoint = POIFactory.generatePOI(grid)
          }

          if (occupiedCell.routes.routeThroughWorkers.isEmpty && occupiedCell.destinationPoint.workerId.value != workerId) {
            setGlobalRoute(occupiedCell, workerId, occupiedCell.destinationPoint.workerId.value, x, y)
          }

          if (occupiedCell.routes.routeToDestination.isEmpty && occupiedCell.destinationPoint.workerId.value == workerId) {
            setLocalRoute(occupiedCell, x, y, grid)
          }



          if (occupiedCell.destinationPoint.workerId.value == workerId) {
            val head::tail = occupiedCell.routes.routeToDestination
            occupiedCell.routes.routeToDestination = tail
            point = head
          } else {
            val nextWorker = occupiedCell.routes.routeThroughWorkers.head
            if (workerId == nextWorker) {
              occupiedCell.routes.routeThroughWorkers = occupiedCell.routes.routeThroughWorkers.tail
            } else {
              try {
                val direction = GlobalAStarAlgorithmUtils.directionValueFromWorkerIds(workerId, nextWorker)
                val smellArray = directionalSmell.apply(direction)(x - 1)(y - 1)
                val directionDifference = GlobalAStarAlgorithmUtils.coordinatesDifferencesListFromSmellArray(smellArray).head
                point = (x + directionDifference._1, y + directionDifference._2)
              } catch {
                case e: Exception => {
                  println("Exception: ", workerId, nextWorker, occupiedCell.routes.routeThroughWorkers)
                  setGlobalRoute(occupiedCell, workerId, occupiedCell.destinationPoint.workerId.value, x, y)
                }
              }
            }
          }

        val destination = point
        val vacatedCell = EmptyCell(cell.smell)

        newGrid.cells(destination._1)(destination._2) match {
          case EmptyCell(_) =>
            newGrid.cells(x)(y) = newGrid.cells(x)(y) match {
              case occupied@MockCell(_, _, _, _, _) => occupied
              case _ => vacatedCell
            }
            newGrid.cells(destination._1)(destination._2) =
              MockCell(
                occupiedCell.smell ++ grid.cells(destination._1)(destination._1).smell,
                occupiedCell.crowd,
                occupiedCell.destinationPoint,
                MockRoutes(
                  occupiedCell.routes.routeThroughWorkers,
                  occupiedCell.routes.routeToDestination
                ),
                occupiedCell.workerId
              )

          case BufferCell(EmptyCell(_)) =>
            newGrid.cells(x)(y) = newGrid.cells(x)(y) match {
              case occupied@MockCell(_, _, _, _, _) => occupied
              case _ => vacatedCell
            }
            newGrid.cells(destination._1)(destination._2) = BufferCell(occupiedCell)

          case BufferCell(another@MockCell(_, anotherCrowd, _, _, _)) =>
            newGrid.cells(x)(y) = newGrid.cells(x)(y) match {
              case occupied@MockCell(_, _, _, _, _) => occupied
              case _ => vacatedCell
            }
            val crowd : List[MockCell] =
              anotherCrowd ++
              List(MockCell.create(
                config.mockInitialSignal,
                destinationPoint = another.destinationPoint,
                workerId = another.workerId
              )) ++
              occupiedCell.crowd

            newGrid.cells(destination._1)(destination._2) =
              BufferCell(
                MockCell.create(config.mockInitialSignal * ((occupiedCell.crowd ++ anotherCrowd).size + 2),
                  crowd,
                  occupiedCell.destinationPoint,
                  MockRoutes(
                    occupiedCell.routes.routeThroughWorkers,
                    occupiedCell.routes.routeToDestination
                  ),
                  grid.workerId
                )
              )

            crowdOnProcessor += 1

          case another@MockCell(_, anotherCrowd, _, _, _) =>
            newGrid.cells(x)(y) = newGrid.cells(x)(y) match {
              case occupied@MockCell(_, _, _, _, _) => occupied
              case _ => vacatedCell
            }
            val crowd : List[MockCell] =
              anotherCrowd ++
                List(MockCell.create(
                  config.mockInitialSignal,
                  destinationPoint = another.destinationPoint,
                  workerId = another.workerId
                )) ++
                occupiedCell.crowd
            newGrid.cells(destination._1)(destination._2) =
              MockCell.create(
                config.mockInitialSignal * ((occupiedCell.crowd ++ anotherCrowd).size + 2),
                crowd,
                occupiedCell.destinationPoint,
                MockRoutes(
                  occupiedCell.routes.routeThroughWorkers,
                  occupiedCell.routes.routeToDestination
                ),
                grid.workerId
              )

            crowdOnProcessor += 1

          case Obstacle() =>
            newGrid.cells(x)(y) = newGrid.cells(x)(y) match {
              case another@MockCell(_, anotherCrowd, _, _, _) =>
                crowdOnProcessor += 1

                val crowd : List[MockCell] =
                  anotherCrowd ++
                    List(MockCell.create(
                      config.mockInitialSignal,
                      destinationPoint = another.destinationPoint,
                      workerId = another.workerId
                    )) ++
                    occupiedCell.crowd
                MockCell.create(
                  config.mockInitialSignal * ((occupiedCell.crowd ++ anotherCrowd).size + 2),
                  crowd,
                  occupiedCell.destinationPoint,
                  MockRoutes(
                    occupiedCell.routes.routeThroughWorkers,
                    occupiedCell.routes.routeToDestination
                  ),
                  grid.workerId
                )
              case _ => occupiedCell
            }

          case _ =>
            throw new UnsupportedOperationException(s"Unresolved move, wtf bro?")
        }
      }

      val mock: MockCell = cell.asInstanceOf[MockCell]

      val newSmell = mock.smell.map {
        arr: Array[Signal] =>
          arr.map {
            sig: Signal => Signal(sig.value + config.mockInitialSignal.value)
          }
      }

      // TODO: splitting whole crowd
      if (mock.crowd.nonEmpty) {
        val singlePedestrianFromCrowd =
          MockCell.create(
            initialSignal = config.mockInitialSignal,
            initialCrowd = mock.crowd.head.crowd,
            destinationPoint = mock.crowd.head.destinationPoint,
            workerId = grid.workerId
          ).withSmell(newSmell)
        makeMockMove(singlePedestrianFromCrowd)

        val mockWithoutOneCrowdPerson =
          MockCell.create(
            initialSignal = config.mockInitialSignal * mock.crowd.size,
            mock.crowd.drop(1),
            mock.destinationPoint,
            workerId = grid.workerId
          ).withSmell(newSmell)

        newGrid.cells(x)(y) = newGrid.cells(x)(y)  match {
          case newPedestrian@MockCell(_,_,_,_,_) =>
            MockCell.create(
              initialSignal = config.mockInitialSignal * mock.crowd.size,
              mockWithoutOneCrowdPerson.crowd ++ List(newPedestrian),
              mock.destinationPoint,
              workerId = grid.workerId
            ).withSmell(newSmell)
           case _ =>
              mockWithoutOneCrowdPerson
        }


      } else {
        val occupiedCell =
          MockCell.create(
            config.mockInitialSignal * (mock.crowd.size + 1),
            mock.crowd,
            mock.destinationPoint,
            MockRoutes(
              mock.routes.routeThroughWorkers,
              mock.routes.routeToDestination
            ),
            workerId = grid.workerId
          ).withSmell(newSmell)
        makeMockMove(occupiedCell)
      }
    }

    val (dynamicCells, staticCells) = (for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } yield (x, y, grid.cells(x)(y))).partition({
      case (_, _, MockCell(_, _, _, _, _)) => true
      case (_, _, _) => false
    })

    staticCells.foreach({ case (x, y, cell) => copyCells(x, y, cell) })
    dynamicCells.foreach({ case (x, y, cell) => moveCells(x, y, cell) })

    val mockPopulation = dynamicCells.foldLeft(0)({ (acc, n) => acc + n._3.asInstanceOf[MockCell].crowd.size + 1 })
    val metrics = MockMetrics(mockPopulation, crowdOnProcessor, 0)
    (newGrid, metrics)
  }

  def isDestinationPointAccessible(grid: Grid, cell: MockCell): Boolean = {
    val point = cell.destinationPoint
    if (point.workerId.value != cell.workerId.value) return true
    grid.cells(point.x)(point.y) match {
      case Obstacle() => false
      case _ => true
    }
  }


}
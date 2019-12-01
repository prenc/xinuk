package pl.edu.agh.mock.utils

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.utils.Direction

import scala.collection.mutable.ListBuffer

object GlobalAStarAlgorithmUtils {

  def coordinatesOfWorkerId(workerId: Int)(implicit config: MockConfig): (Int, Int) = {
    val globalY = (workerId - 1) / config.workersRoot
    val globalX = (workerId - 1) % config.workersRoot
    (globalX, globalY)
  }

  def workerIdOfCoordinates(x: Int, y: Int)(implicit config: MockConfig): Int = {
    val workerId = (y * config.workersRoot) + x + 1
    workerId
  }

  def neighboursOf(workerId: Int)(implicit config: MockConfig): List[Int] = {
    val coordinates = coordinatesOfWorkerId(workerId)
    differenceCoordinates()
      .filter(x => !(x._1 == 0 && x._2 == 0))
      .map(x => (x._1 + coordinates._1, x._2 + coordinates._2))
      .filter(x => !(x._1 < 0 || x._2 < 0 || x._1 > config.workersRoot - 1 || x._2 > config.workersRoot - 1))
      .map(coordinate => workerIdOfCoordinates(coordinate._1, coordinate._2))
  }

  def differenceCoordinates(): List[(Int, Int)] = {
    val neighbourSubtractors = List(-1,0,1)
    for(x <- neighbourSubtractors; y <- neighbourSubtractors) yield (x, y)
  }

  def accessibleNeighboursOf(workerId: Int, from: Int, transitions: Map[Int, Map[Direction.Value, List[Direction.Value]]])(implicit config: MockConfig): List[Int] = {
    transitions(workerId)(directionValueFromWorkerIds(from, workerId)).map(direction => workerIdsFromDirectionValues(workerId, direction))
  }

  def calculateDirection(smellArray: SmellArray, directionalSmellArray: SmellArray)(implicit config: MockConfig): List[(Int, Int)] = {
    val smellIterator =
      differenceCoordinates()
        .filter(x => !(x._1 == 0 && x._2 == 0))
        .map(cord => (cord._1, cord._2, smellArray(cord._1 + 1)(cord._2 + 1).value))
    val directionIterator =
      differenceCoordinates()
        .filter(x => !(x._1 == 0 && x._2 == 0))
        .map(cord => (cord._1, cord._2, directionalSmellArray(cord._1 + 1)(cord._2 + 1).value))

    smellIterator
      .zip(directionIterator)
      .map {
        case ((i, j, smell),(_, _, distance)) =>
          (i, j, smell, distance)
      }
      .filter(x => !(x._1 == 0 && x._2 == 0))
      .map {
        case (i, j, smell, distance) =>
          (i, j, config.distanceFactor * distance + config.repulsionFactor * smell)
      }
      .sortBy(_._3)
      .reverse
      .map(x => (x._1, x._2))
  }

//  def coordinatesDifferencesListFromSmellArray(smellArray: SmellArray): List[(Int, Int)] = {
//    differenceCoordinates()
//      .filter(x => !(x._1 == 0 && x._2 == 0))
//      .map(cord => (cord._1, cord._2, smellArray(cord._1 + 1)(cord._2 + 1).value))
//      .sortBy(_._3)
//      .reverse
//      .map(x => (x._1, x._2))
//  }

  def directionValueFromWorkerIds(from: Int, through: Int)(implicit config: MockConfig): Direction.Value = {
    val fromCoordinates = coordinatesOfWorkerId(from)
    val throughCoordinates = coordinatesOfWorkerId(through)
    val difference = (throughCoordinates._1 - fromCoordinates._1, throughCoordinates._2 - fromCoordinates._2)
    differenceCoordinatesToDirection(difference)
  }

  def differenceCoordinatesToDirection(difference: (Int, Int)): Direction.Value = {
    difference match {
      case (-1, -1) => Direction.TopLeft
      case (-1, 0) => Direction.Left
      case (-1, 1) => Direction.BottomLeft
      case (0, -1) => Direction.Top
      case (0, 1) => Direction.Bottom
      case (1, -1) => Direction.TopRight
      case (1, 0) => Direction.Right
      case (1, 1) => Direction.BottomRight
      case (0, 0) => throw new Exception("No direction for 0, 0")
      case _ => throw new Exception("Bad coordinates")
    }
  }

  def directionToDifferenceCoordinates(direction: Direction.Value): (Int, Int) = {
    direction match {
      case Direction.TopLeft => (-1, -1)
      case Direction.Top => (0, -1)
      case Direction.TopRight => (1, -1)
      case Direction.Left => (-1, 0)
      case Direction.Right => (1, 0)
      case Direction.BottomLeft => (-1, 1)
      case Direction.Bottom => (0, 1)
      case Direction.BottomRight => (1, 1)
    }
  }

  def workerIdsFromDirectionValues(through: Int, toDirection: Direction.Value)(implicit config: MockConfig): Int = {
    val coordinatesDifference = directionToDifferenceCoordinates(toDirection)
    val throughCoordinates = coordinatesOfWorkerId(through)
    val resultCoordinates = (throughCoordinates._1 + coordinatesDifference._1, throughCoordinates._2 + coordinatesDifference._2)
    if (
      resultCoordinates._1 < 0 || resultCoordinates._2 > config.workersRoot - 1 ||
      resultCoordinates._2 < 0 || resultCoordinates._2 > config.workersRoot - 1
    ) {
      return 0
    }
    workerIdOfCoordinates(resultCoordinates._1, resultCoordinates._2)
  }

  def accessibleNeighboursForFirstIteration(
                                             workerId: Int,
                                             x: Int,
                                             y: Int,
                                             directionalSmell: Map[Direction.Value, Array[Array[SmellArray]]]
                                           )(implicit config: MockConfig): List[Int] = {
    var listOfDirections = ListBuffer[Direction.Value]()
    for (direction <- directionalSmell.keys) {
      if (directionalSmell(direction)(x - 1)(y - 1).flatten.count(signal => signal.value != 0) != 0) {
        listOfDirections += direction
      }
    }
    listOfDirections
      .map(direction => workerIdsFromDirectionValues(workerId, direction))
      .filter(id => id >= 1 && id <= math.pow(config.workersRoot, 2))
      .filter(id => id != workerId)
      .toList
  }

  def aStar(
             start: Int,
             goal: Int,
             x: Int,
             y: Int,
             directionalSmell: Map[Direction.Value, Array[Array[SmellArray]]],
             transitions: Map[Int, Map[Direction.Value, List[Direction.Value]]]
           )(implicit config: MockConfig): List[Int] = {
    var openSet: Set[Int] = Set[Int]()
    openSet += start

    var cameFrom = Map[Int, Int]()

    var gScore = Map[Int, Double]().withDefaultValue(Double.MaxValue)
    gScore += (start -> 0)

    var fScore = Map[Int, Double]().withDefaultValue(Double.MaxValue)
    fScore += (start -> heuristic(start, goal, transitions))

    while (openSet.nonEmpty) {
      val current = nodeWithLowestFScoreValue(openSet, fScore)
      if(current == goal) {
        return reconstructPath(cameFrom, current, goal)
      }
      openSet -= current

      var neighbours: List[Int] = List[Int]()

      if (cameFrom.isEmpty) {
        neighbours = accessibleNeighboursForFirstIteration(start, x, y, directionalSmell)
      } else {
        if (current != cameFrom(current)) {
          neighbours = accessibleNeighboursOf(current, cameFrom(current), transitions)
        }
      }

      for (neighbour <- neighbours) {
        val tentativeGScore = gScore(current) + distance(current, neighbour)
        if (tentativeGScore < gScore(neighbour)) {
          cameFrom -= neighbour
          cameFrom += (neighbour -> current)
          gScore -= neighbour
          gScore += (neighbour -> tentativeGScore)
          fScore -= neighbour
          fScore += (neighbour -> (gScore(neighbour) + heuristic(neighbour, goal, transitions)))
          if (!openSet.contains(neighbour)) {
            openSet += neighbour
          }
        }
      }
    }
    throw new Exception("A* failure")
  }

  def heuristic(worker: Int, goal: Int, transitions: Map[Int, Map[Direction.Value, List[Direction.Value]]])(implicit config: MockConfig): Double = {
    val workerCoordinates = coordinatesOfWorkerId(worker)
    val goalCoordinates = coordinatesOfWorkerId(goal)
    Math.sqrt(Math.pow(workerCoordinates._1 - goalCoordinates._1, 2) + Math.pow(workerCoordinates._2 - goalCoordinates._2, 2))
  }

  def nodeWithLowestFScoreValue(openSet: Set[Int], fScore: Map[Int, Double]): Int = {
    openSet.map(openEntry => (openEntry, fScore(openEntry))).minBy(_._2)._1
  }

  def reconstructPath(cameFrom: Map[Int, Int], current: Int, goal: Int): List[Int] = {
    var currentNode: Int = current
    val totalPath = ListBuffer[Int]()

    while (cameFrom.contains(currentNode)) {
      currentNode = cameFrom.apply(currentNode)
      totalPath.prepend(currentNode)
    }

    totalPath.append(goal)
    totalPath.tail.toList
  }

  def distance(current: Int, neighbour: Int)(implicit config: MockConfig): Double = {
    val currentCoordinates = coordinatesOfWorkerId(current)
    val neighbourCoordinates = coordinatesOfWorkerId(neighbour)
    val difference = (currentCoordinates._1 - neighbourCoordinates._1, currentCoordinates._2 - neighbourCoordinates._2)
    if (difference._1 == 0 || difference._2 == 0) {
      1.0
    } else {
      1.41421
    }
  }
}

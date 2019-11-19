package pl.edu.agh.mock.utils

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.utlis.Direction
import scala.collection.mutable.ListBuffer

object AlgorithmHelpers {
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
    val neighbourSubtractors = List(-1,0,1)
    val everyWithEvery = for(x <- neighbourSubtractors; y <- neighbourSubtractors) yield (x, y)
    everyWithEvery
      .filter(x => !(x._1 == 0 && x._2 == 0))
      .map(x => (x._1 + coordinates._1, x._2 + coordinates._2))
      .filter(x => !(x._1 < 0 || x._2 < 0 || x._1 > config.workersRoot || x._2 > config.workersRoot))
      .map(coordinate => workerIdOfCoordinates(coordinate._1, coordinate._2))
  }

  def accessibleNeighboursOf(workerId: Int, from: Int, transitions: Map[Int, Map[Direction.Value, List[Direction.Value]]])(implicit config: MockConfig): List[Int] = {
    transitions(workerId)(directionValuesFromWorkerIds(from, workerId)).map(direction => workerIdsFromDirectionValues(workerId, direction))
  }

  def directionValuesFromWorkerIds(from: Int, through: Int)(implicit config: MockConfig): Direction.Value = {
    val fromCoordinates = coordinatesOfWorkerId(from)
    val throughCoordinates = coordinatesOfWorkerId(through)
    val difference = (throughCoordinates._1 - fromCoordinates._1, throughCoordinates._2 - fromCoordinates._2)
    difference match {
      case (-1, -1) => Direction.TopLeft
      case (-1, 0) => Direction.Left
      case (-1, 1) => Direction.BottomLeft
      case (0, -1) => Direction.Top
      case (0, 1) => Direction.Bottom
      case (1, -1) => Direction.TopRight
      case (1, 0) => Direction.Right
      case (1, 1) => Direction.BottomRight
      case _ => throw new Exception("Bad direction value")
    }
  }

  def workerIdsFromDirectionValues(through: Int, toDirection: Direction.Value)(implicit config: MockConfig): Int = {
    val coordinatesDifference = toDirection match {
      case Direction.TopLeft => (-1, -1)
      case Direction.Top => (0, -1)
      case Direction.TopRight => (1, -1)
      case Direction.Left => (-1, 0)
      case Direction.Right => (1, 0)
      case Direction.BottomLeft => (-1, 1)
      case Direction.Bottom => (0, 1)
      case Direction.BottomRight => (1, 1)
    }
    val throughCoordinates = coordinatesOfWorkerId(through)
    workerIdOfCoordinates(throughCoordinates._1 + coordinatesDifference._1, throughCoordinates._2 + coordinatesDifference._2)
  }

  def aStar(start: Int, goal: Int, transitions: Map[Int, Map[Direction.Value, List[Direction.Value]]])(implicit config: MockConfig): List[Int] = {
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
        return reconstructPath(cameFrom, current)
      }
      openSet -= current


      for (neighbour <- accessibleNeighboursOf(current, cameFrom(current), transitions)) {
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

  def reconstructPath(cameFrom: Map[Int, Int], current: Int): List[Int] = {
    var currentNode: Int = current
    val totalPath = ListBuffer[Int]()
    while (cameFrom.contains(currentNode)) {
      currentNode = cameFrom.apply(currentNode)
      totalPath.prepend(currentNode)
    }
    totalPath.toList
  }

  def distance(current: Int, neighbour: Int): Double = {
    1.0
  }
}

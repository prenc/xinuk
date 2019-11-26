package pl.edu.agh.mock.utlis

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.xinuk.model.{Grid, Obstacle}

import scala.collection.mutable.ListBuffer

object LocalAStarAlgorithmUtils {

  def aStar(start: (Int, Int), goal: (Int, Int), grid: Grid)(implicit config: MockConfig): List[(Int, Int)] = {
    var openSet: Set[(Int, Int)] = Set[(Int, Int)]()
    openSet += start

    var cameFrom = Map[(Int, Int), (Int, Int)]()

    var gScore = Map[(Int, Int), Double]().withDefaultValue(Double.MaxValue)
    gScore += (start -> 0)

    var fScore = Map[(Int, Int), Double]().withDefaultValue(Double.MaxValue)
    fScore += (start -> heuristic(start, goal, grid))

    while (openSet.nonEmpty) {
      val current = nodeWithLowestFScoreValue(openSet, fScore)
      if(current == goal) {
        return reconstructPath(cameFrom, current, goal)
      }
      openSet -= current

      for (neighbour <- neighbours(current, grid)) {
        val tentativeGScore = gScore(current) + distance(current, neighbour)
        if (tentativeGScore < gScore(neighbour)) {
          cameFrom -= neighbour
          cameFrom += (neighbour -> current)
          gScore -= neighbour
          gScore += (neighbour -> tentativeGScore)
          fScore -= neighbour
          fScore += (neighbour -> (gScore(neighbour) + heuristic(neighbour, goal, grid)))
          if (!openSet.contains(neighbour)) {
            openSet += neighbour
          }
        }
      }
    }
    throw new Exception("A* failure")
  }

  def heuristic(node: (Int, Int), goal: (Int, Int), grid: Grid): Double = {
    Math.sqrt(Math.pow(node._1 - goal._1, 2) + Math.pow(node._2 - goal._2, 2))
  }

  def nodeWithLowestFScoreValue(openSet: Set[(Int, Int)], fScore: Map[(Int, Int), Double]): (Int, Int) = {
    openSet.map(openEntry => (openEntry, fScore(openEntry))).minBy(_._2)._1
  }

  def reconstructPath(cameFrom: Map[(Int, Int), (Int, Int)], current: (Int, Int), goal: (Int,Int)): List[(Int, Int)] = {
    var currentNode: (Int, Int) = current
    val totalPath = ListBuffer[(Int, Int)]()
    while (cameFrom.contains(currentNode)) {
      currentNode = cameFrom.apply(currentNode)
      totalPath.prepend(currentNode)
    }
    totalPath.append(goal)
    totalPath.tail.toList
  }

  def neighbours(node: (Int, Int),  grid: Grid)(implicit config: MockConfig): List[(Int, Int)] = {
    val neighbourSubtractors = List(-1,0,1)
    val everyWithEvery = for(x <- neighbourSubtractors; y <- neighbourSubtractors) yield (x, y)
    val neighbourCoordinates = everyWithEvery
      .filter(x => !(x._1 == 0 && x._2 == 0))
      .map(x => (x._1 + node._1, x._2 + node._2))
      .filter(x => !(x._1 < 0 || x._2 < 0 || x._1 > config.gridSize - 1 || x._2 > config.gridSize - 1))
      .filter(coordinates => grid.cells(coordinates._1)(coordinates._2) != Obstacle())
    neighbourCoordinates
  }

  def distance(current: (Int, Int), neighbour: (Int, Int)): Double = {
    val difference = (current._1 - neighbour._1, current._2 - neighbour._2)
    if (difference._1 == 0 || difference._2 == 0) {
      1.0
    } else {
      1.41421
    }
  }
}

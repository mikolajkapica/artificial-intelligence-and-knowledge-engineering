package algorithms

import algorithms.SingleEndStopPathFindingAlgorithm.*
import algorithms.MultipleStopsPathFindingAlgorithm.*
import domain.Connection
import domain.Graph
import domain.Stop
import domain.Time

import scala.collection.mutable

sealed trait PathFindingAlgorithm

sealed trait SingleEndStopPathFindingAlgorithm extends PathFindingAlgorithm

object SingleEndStopPathFindingAlgorithm:
  case object AStar extends SingleEndStopPathFindingAlgorithm
  case object Dijkstra extends SingleEndStopPathFindingAlgorithm

def findShortestPath(
  algorithm: SingleEndStopPathFindingAlgorithm,
  graph: Graph,
  start: Stop,
  startTime: Time,
  end: Stop,
  costFunction: CostFunction,
): Option[PathFindingResult] = algorithm match
  case AStar    => AStarImpl.run(start, startTime, end, graph, costFunction)
  case Dijkstra => DijkstraImpl.run(start, startTime, end, graph, costFunction)

sealed trait MultipleStopsPathFindingAlgorithm extends PathFindingAlgorithm

object MultipleStopsPathFindingAlgorithm:
  case object TabuSearch extends MultipleStopsPathFindingAlgorithm

def findShortestPath(
  algorithm: MultipleStopsPathFindingAlgorithm,
  graph: Graph,
  start: Stop,
  startTime: Time,
  through: List[Stop],
  cost: CostFunction,
): Option[PathFindingResult] = algorithm match
  case TabuSearch => TabuSearchKnox.run(start, startTime, through, graph, cost)

case class PathFindingResult(
  path: List[Connection],
  cost: Double,
)

def reconstructPath(
  predecessors: Map[Stop, Connection],
  end: Stop,
): List[Connection] = {
  val path = mutable.ListBuffer.empty[Connection]
  var current = end

  while (predecessors.contains(current)) {
    val parent = predecessors(current)
    current = parent.startStop
    path.prepend(parent)
  }

  path.toList
}


package algorithms

import algorithms.SingleEndStopPathFindingAlgorithm.*
import algorithms.MultipleStopsPathFindingAlgorithm.*
import domain.Connection
import domain.Graph
import domain.Stop

sealed trait PathFindingAlgorithm

sealed trait SingleEndStopPathFindingAlgorithm extends PathFindingAlgorithm

object SingleEndStopPathFindingAlgorithm:
  case object AStar extends SingleEndStopPathFindingAlgorithm
  case object Dijkstra extends SingleEndStopPathFindingAlgorithm

def findShortestPath(
  algorithm: SingleEndStopPathFindingAlgorithm,
  graph: Graph,
  start: Stop,
  end: Stop,
  cost: (Stop, Stop) => Double,
): Option[PathFindingResult] = algorithm match
  case AStar    => AStarImpl.run(start, end, graph, cost)
  case Dijkstra => DijkstraImpl.run(start, end, graph, cost)

sealed trait MultipleStopsPathFindingAlgorithm extends PathFindingAlgorithm

object MultipleStopsPathFindingAlgorithm:
  case object TabuSearch extends MultipleStopsPathFindingAlgorithm

def findShortestPath(
  algorithm: MultipleStopsPathFindingAlgorithm,
  graph: Graph,
  start: Stop,
  through: List[Stop],
  cost: (Stop, Stop) => Double,
): Option[PathFindingResult] = algorithm match
  case TabuSearch => TabuSearchKnox.run(start, through, graph, cost)

case class PathFindingResult(
  path: List[Connection],
  cost: Double,
)

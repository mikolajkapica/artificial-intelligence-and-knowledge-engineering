package utils

import algorithms.*
import CostFunctions.CostFunction
import MultipleStopsPathFindingAlgorithm.*
import SingleEndStopPathFindingAlgorithm.*
import domain.Connection
import domain.Graph
import domain.Stop
import domain.Time

sealed trait PathFindingAlgorithm

sealed trait SingleEndStopPathFindingAlgorithm extends PathFindingAlgorithm

object SingleEndStopPathFindingAlgorithm:
  case object AStar extends SingleEndStopPathFindingAlgorithm
  case object AStarOptimized extends SingleEndStopPathFindingAlgorithm
  case object Dijkstra extends SingleEndStopPathFindingAlgorithm

def findShortestPath(
  algorithm: SingleEndStopPathFindingAlgorithm,
  graph: Graph,
  start: Stop,
  startTime: Time,
  end: Stop,
  costFunction: CostFunction,
  heuristic: (Stop, Stop) => Double,
): Option[PathFindingResult] = algorithm match
  case AStar          => AStarImpl.run(start, startTime, end, graph, costFunction, heuristic)
  case AStarOptimized => AStarOptimizedImpl.run(start, startTime, end, graph, costFunction, heuristic)
  case Dijkstra       => DijkstraImpl.run(start, startTime, end, graph, costFunction)

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
  heuristic: (Stop, Stop) => Double,
): Option[PathFindingResult] = algorithm match
  case TabuSearch => TabuSearchImpl.run(start, startTime, through, graph, cost, heuristic)

case class PathFindingResult(
  path: List[Connection],
  cost: Double,
)

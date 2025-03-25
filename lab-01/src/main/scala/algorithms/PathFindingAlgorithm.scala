package algorithms

import cats.implicits.catsSyntaxEq
import domain.Connection
import domain.Graph
import domain.Stop
import domain.Stop

def timeCost(graph: Graph)(a: Stop, b: Stop): Double =
  graph(a).find(_.endStop === b).map(_.arrivalTime.hour.toDouble).getOrElse(Double.MaxValue)

def transfersCost(graph: Graph)(a: Stop, b: Stop): Double =
  if (a === b) 0 else 1

def lengthHeuristic(a: Stop, b: Stop): Double =
  b.coordinates.latitude - a.coordinates.latitude + b.coordinates.longitude - a.coordinates.longitude

enum PathFindingAlgorithm:
  case AStar, Dijkstra

case class PathFindingResult(
                              path: List[Connection],
                              cost: Double,
)

def findShortestPath(
  algorithm: PathFindingAlgorithm,
  graph: Graph,
  start: Stop,
  end: Stop,
): Option[PathFindingResult] = algorithm match
  case PathFindingAlgorithm.AStar    => AStarImplementation.run(start, end, graph)
  case PathFindingAlgorithm.Dijkstra => DijkstraImplementation.run(start, end, graph)

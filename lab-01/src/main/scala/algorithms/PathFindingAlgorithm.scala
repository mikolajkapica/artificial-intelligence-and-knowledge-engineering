package algorithms

import cats.implicits.catsSyntaxEq
import domain.Connection
import domain.Graph
import domain.Stop

type CostFunction = Graph => (Stop, Stop) => Double

def timeCost: CostFunction =
  graph => (a, b) => graph(a).filter(_.endStop === b).map(_.arrivalTime.hour.toDouble).minOption.getOrElse(Double.MaxValue)

def transfersCost: CostFunction =
  graph => (a, b) => if a === b then 0 else 1

enum Optimization:
  case Time, Transfers

def cost: Optimization => CostFunction = {
  case Optimization.Time      => timeCost
  case Optimization.Transfers => transfersCost
}

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
  cost: (Stop, Stop) => Double,
): Option[PathFindingResult] = algorithm match
  case PathFindingAlgorithm.AStar    => AStarImplementation.run(start, end, graph, cost)
  case PathFindingAlgorithm.Dijkstra => DijkstraImplementation.run(start, end, graph, cost)

package algorithms

import cats.implicits.catsSyntaxEq
import domain.{Graph, Stop}

enum Optimization:
  case Time
  case Transfers

def getCostFunction: Optimization => Graph => CostFunction = {
  case Optimization.Time      => timeCost
  case Optimization.Transfers => transfersCost
}

type CostFunction = (Stop, Stop) => Double

def timeCost: Graph => CostFunction =
  graph => (a, b) => graph(a).filter(_.endStop === b).map(_.arrivalTime.hour.toDouble).minOption.getOrElse(Double.MaxValue)

def transfersCost: Graph => CostFunction =
  graph => (a, b) => if a === b then 0 else 1


def lengthHeuristic(a: Stop, b: Stop): Double =
  b.coordinates.latitude - a.coordinates.latitude + b.coordinates.longitude - a.coordinates.longitude

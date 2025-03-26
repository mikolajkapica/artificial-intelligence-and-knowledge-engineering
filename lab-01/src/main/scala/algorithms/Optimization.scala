package algorithms

import cats.implicits.catsSyntaxEq
import domain.Connection
import domain.Graph
import domain.Stop
import domain.Time

enum Optimization:
  case Time
  case Transfers

def lengthHeuristic(a: Stop, b: Stop): Double =
  b.coordinates.latitude - a.coordinates.latitude + b.coordinates.longitude - a.coordinates.longitude

type CostFunction = (Time, Option[Connection], Connection) => Double

def timeCost(startTime: Time, fromOpt: Option[Connection], through: Connection): Double = {
  val travelTime = through.departureTime.to(through.arrivalTime)
  fromOpt.fold(startTime.to(through.departureTime) + travelTime) { from =>
    val waitingTime = from.arrivalTime.to(through.departureTime)
    travelTime + waitingTime
  }
}

def transfersCost(startTime: Time, fromOpt: Option[Connection], through: Connection): Double =
  fromOpt.fold(1)(from => if from.line == through.line then 0 else 1)

def getCostFunction: Optimization => CostFunction = {
  case Optimization.Time      => timeCost
  case Optimization.Transfers => transfersCost
}

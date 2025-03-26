package algorithms.utils

import domain.{Connection, Stop, Time}
import scala.concurrent.duration.DurationInt

object CostFunctions {
  type CostFunction = (Time, Option[Connection], Connection) => Double

  def timeCost(startTime: Time, fromOpt: Option[Connection], through: Connection): Double = {
    val travelTime = through.departureTime.to(through.arrivalTime)
    fromOpt.fold {
      val waitingTime = startTime.to(through.departureTime)
      (waitingTime + travelTime).toSeconds.toDouble
    } { from =>
      val waitingTime = from.arrivalTime.to(through.departureTime)
      (travelTime + waitingTime).toSeconds.toDouble
    }
  }

  def transfersCost(startTime: Time, fromOpt: Option[Connection], through: Connection): Double =
    fromOpt.fold(1)(from => if from.line == through.line && from.arrivalTime.to(through.departureTime) < 5.minutes then 0 else 1)

  // a cost function that takes both time and transfers into account
  def combinedCost(startTime: Time, fromOpt: Option[Connection], through: Connection): Double =
    timeCost(startTime, fromOpt, through) + transfersCost(startTime, fromOpt, through) * 1000

  def getCostFunction: Optimization => CostFunction = {
    case Optimization.Time      => timeCost
    case Optimization.Transfers => transfersCost
    case Optimization.Combined  => combinedCost
  }

}

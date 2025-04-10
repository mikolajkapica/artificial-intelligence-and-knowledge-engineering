package utils

import domain.Connection
import domain.Time
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
    fromOpt.fold(1)(from =>
      if from.line == through.line || from.arrivalTime.to(through.departureTime) < 5.minutes then 0 else 1
    )

  def combinedCost(startTime: Time, fromOpt: Option[Connection], through: Connection): Double = {
    val waitingTime =
      fromOpt.fold(startTime.to(through.departureTime))(from => from.arrivalTime.to(through.departureTime)).toSeconds
    timeCost(startTime, fromOpt, through) + transfersCost(startTime, fromOpt, through) * waitingTime
  }

  def getCostFunction: Optimization => CostFunction = {
    case Optimization.Time      => timeCost
    case Optimization.Transfers => transfersCost
    case Optimization.Combined  => combinedCost
  }

}

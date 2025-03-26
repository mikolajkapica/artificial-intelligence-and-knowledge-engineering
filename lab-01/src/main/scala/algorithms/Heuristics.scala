package algorithms

import domain.Distance.haversineDistance
import domain.Stop
import domain.WGS84

import scala.math.*

object Heuristics {

  private val EARTH_RADIUS_METERS = 6371e3

  private val MAX_SPEED_METERS_PER_SECOND: Double = 33.33

  enum Heuristic {
    case Zero
    case TimeHaversine
    case DistanceHaversine
    case ManhattanDegree
    case EuclideanDegree
  }

  import Heuristic.*

  def getHeuristic(heuristic: Heuristic): (Stop, Stop) => Double = heuristic match {
    case Zero              => zeroHeuristic
    case TimeHaversine     => timeHaversineHeuristic
    case DistanceHaversine => distanceHaversineHeuristic
    case ManhattanDegree   => manhattanDegreeHeuristic
    case EuclideanDegree   => euclideanDegreeHeuristic
  }

  private def zeroHeuristic(from: Stop, to: Stop): Double = 0.0

  private def timeHaversineHeuristic(from: Stop, to: Stop): Double =
    if (from == to) {
      0.0
    } else {
      val distance = haversineDistance(from.coordinates, to.coordinates)
      distance / MAX_SPEED_METERS_PER_SECOND
    }

  private def distanceHaversineHeuristic(from: Stop, to: Stop): Double =
    if (from == to) {
      0.0
    } else {
      haversineDistance(from.coordinates, to.coordinates)
    }

  private def manhattanDegreeHeuristic(from: Stop, to: Stop): Double = {
    val dLat = abs(to.coordinates.latitude - from.coordinates.latitude)
    val dLon = abs(to.coordinates.longitude - from.coordinates.longitude)
    dLat + dLon
  }

  private def euclideanDegreeHeuristic(from: Stop, to: Stop): Double = {
    val dLat = to.coordinates.latitude - from.coordinates.latitude
    val dLon = to.coordinates.longitude - from.coordinates.longitude
    sqrt(pow(dLat, 2) + pow(dLon, 2))
  }

}

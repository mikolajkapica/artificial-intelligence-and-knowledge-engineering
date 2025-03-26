package domain

import cats.Eq
import cats.syntax.all.*
import cats.effect.IO
import cats.implicits.catsSyntaxEq

import scala.math._

case class Stop(name: String, coordinates: WGS84) {

  private val prec = 1

  private val latNorm: Int = (coordinates.latitude * math.pow(10, prec)).toInt

  private val lonNorm: Int = (coordinates.longitude * math.pow(10, prec)).toInt

  def distanceTo(to: Stop): Double = coordinates.distanceTo(to.coordinates)

  override def equals(obj: Any): Boolean = obj match {
    case other: Stop =>
      this.name == other.name &&
      this.latNorm == other.latNorm &&
      this.lonNorm == other.lonNorm
    case _           => false
  }

  override def hashCode: Int = (name, latNorm, lonNorm).##
}

object Stop:
  given Eq[Stop] = Eq.fromUniversalEquals

  def parse(
    stop: String,
    graph: Graph,
  ): Either[String, Stop] =
    graph.keys.find(_.name === stop).toRight(s"Stop $stop not found")

  def parseIO(
    stop: String,
    graph: Graph,
  ): IO[Stop] =
    IO.fromEither(parse(stop, graph).leftMap(new Exception(_)))

case class WGS84(latitude: Double, longitude: Double) {

  def distanceTo(to: WGS84): Double = {
    // Earth's mean radius in meters (approximation)
    val R = 6371000.0

    // Convert latitude and longitude from degrees to radians
    val lat1Rad = toRadians(this.latitude)
    val lon1Rad = toRadians(this.longitude)
    val lat2Rad = toRadians(to.latitude)
    val lon2Rad = toRadians(to.longitude)

    // Differences in coordinates
    val dLat = lat2Rad - lat1Rad
    val dLon = lon2Rad - lon1Rad

    // Haversine formula calculation
    val a = pow(sin(dLat / 2), 2) + cos(lat1Rad) * cos(lat2Rad) * pow(sin(dLon / 2), 2)
    val c = 2 * atan2(sqrt(a), sqrt(1 - a))

    // Calculate the distance
    val distance = R * c
    distance
  }

}

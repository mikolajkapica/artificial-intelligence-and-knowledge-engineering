package domain

import cats.Eq
import cats.effect.IO
import cats.implicits.catsSyntaxEq
import cats.syntax.all.*

import scala.math.*

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

  def distanceTo(to: WGS84): Double = Distance.haversineDistance(this, to)

}

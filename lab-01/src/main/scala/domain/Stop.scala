package domain

import cats.Eq
import cats.syntax.all.*
import cats.effect.IO
import cats.implicits.catsSyntaxEq

case class Stop(name: String, coordinates: WGS84)

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

case class WGS84(latitude: Double, longitude: Double)



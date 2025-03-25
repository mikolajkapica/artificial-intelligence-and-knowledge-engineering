package domain

import cats.Eq

case class Stop(name: String, coordinates: WGS84)

object Stop:
  given Eq[Stop] = Eq.fromUniversalEquals

case class WGS84(latitude: Double, longitude: Double)

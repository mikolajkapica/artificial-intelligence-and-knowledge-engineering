package domain

import domain.WGS84

import scala.math.toRadians

object Distance {
  private val EARTH_RADIUS_METERS = 6371e3

  def haversineDistance(coord1: WGS84, coord2: WGS84): Double = {
    val lat1Rad = toRadians(coord1.latitude)
    val lon1Rad = toRadians(coord1.longitude)
    val lat2Rad = toRadians(coord2.latitude)
    val lon2Rad = toRadians(coord2.longitude)

    val dLat = lat2Rad - lat1Rad
    val dLon = lon2Rad - lon1Rad

    val a = math.pow(math.sin(dLat / 2), 2) + math.cos(lat1Rad) * math.cos(lat2Rad) * math.pow(
      math.sin(dLon / 2),
      2,
    )
    val c = 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))

    EARTH_RADIUS_METERS * c
  }
}
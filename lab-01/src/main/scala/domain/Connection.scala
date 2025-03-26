package domain

import cats.data.NonEmptyList
import cats.implicits.*
import cats.kernel.Eq
import cats.syntax.either.*
import fs2.data.csv.*
import fs2.data.csv.generic.semiauto.*


case class Connection(
  company: String,
  line: String,
  departureTime: Time,
  arrivalTime: Time,
  startStop: Stop,
  endStop: Stop,
) {
  def distance: Double =
    endStop.distanceTo(startStop)
}

object Connection:

  given csvRowDecoder: CsvRowDecoder[Connection, String] with

    def apply(row: CsvRow[String]): DecoderResult[Connection] =
      for {
        _              <- row.as[Long]("")
        company        <- row.as[String]("company")
        line           <- row.as[String]("line")
        departure_time <- row.as[Time]("departure_time")
        arrival_time   <- row.as[Time]("arrival_time")
        start_stop     <- row.as[String]("start_stop")
        end_stop       <- row.as[String]("end_stop")
        start_stop_lat <- row.as[Double]("start_stop_lat")
        start_stop_lon <- row.as[Double]("start_stop_lon")
        end_stop_lat   <- row.as[Double]("end_stop_lat")
        end_stop_lon   <- row.as[Double]("end_stop_lon")
      } yield Connection(
        company = company,
        line = line,
        departureTime = departure_time,
        arrivalTime = arrival_time,
        startStop = Stop(start_stop, WGS84(start_stop_lat, start_stop_lon)),
        endStop = Stop(end_stop, WGS84(end_stop_lat, end_stop_lon)),
      )

  given csvRowEncoder: CsvRowEncoder[Connection, String] with

    def apply(bcs: Connection): CsvRow[String] =
      CsvRow.fromNelHeaders(
        NonEmptyList.of(
          ("company", bcs.company),
          ("line", bcs.line),
          ("departure_time", bcs.departureTime.toString),
          ("arrival_time", bcs.arrivalTime.toString),
          ("start_stop", bcs.startStop.name),
          ("end_stop", bcs.endStop.name),
          ("start_stop_lat", bcs.startStop.coordinates.latitude.toString),
          ("start_stop_lon", bcs.startStop.coordinates.longitude.toString),
          ("end_stop_lat", bcs.endStop.coordinates.latitude.toString),
          ("end_stop_lon", bcs.endStop.coordinates.longitude.toString),
        )
      )

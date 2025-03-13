package domain

import cats.Show
import cats.data.NonEmptyList
import cats.implicits._
import cats.kernel.Eq
import cats.syntax.all._
import cats.syntax.either._

import fs2.data.csv.CellDecoder
import fs2.data.csv.CsvRow
import fs2.data.csv.CsvRowDecoder
import fs2.data.csv.CsvRowEncoder
import fs2.data.csv.DecoderError
import fs2.data.csv.DecoderResult
import fs2.data.csv.generic.semiauto._

type Graph = Map[Stop, Set[BusConnectionSample]]

case class BusConnectionSample(
  company: String,
  line: String,
  departureTime: Time,
  arrivalTime: Time,
  startStop: Stop,
  endStop: Stop,
)

case class Stop(name: String, coordinates: WGS84)

object Stop:
  given Eq[Stop] = Eq.fromUniversalEquals

case class WGS84(latitude: Double, longitude: Double) // TODO: Newtypes

case class Time(hour: Int, minute: Int, seconds: Int) extends Product, Serializable

object Time {

  implicit val cellDecoder: CellDecoder[Time] = CellDecoder.stringDecoder.emap { str =>
    val a = str.split(":") match {
      case Array(h, m, s) =>
        for {
          hour   <- h.toIntOption.toRight(s"Invalid hour: $h").map(_ % 24)
          minute <- m.toIntOption.toRight(s"Invalid minute: $m")
          second <- m.toIntOption.toRight(s"Invalid minute: $m")
          _      <- Either.cond(minute >= 0 && minute < 60, (), s"Minute out of range: $str")
          _      <- Either.cond(second >= 0 && second < 60, (), s"Second out of range: $str")
        } yield Time(hour, minute, second)
      case _              => Left(s"Invalid time format: $str")
    }
    a.leftMap(DecoderError(_))
  }

}

object BusConnectionSample:

  given csvRowDecoder: CsvRowDecoder[BusConnectionSample, String] with

    def apply(row: CsvRow[String]): DecoderResult[BusConnectionSample] =
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
      } yield BusConnectionSample(
        company = company,
        line = line,
        departureTime = departure_time,
        arrivalTime = arrival_time,
        startStop = Stop(start_stop, WGS84(start_stop_lat, start_stop_lon)),
        endStop = Stop(end_stop, WGS84(end_stop_lat, end_stop_lon)),
      )

  given csvRowEncoder: CsvRowEncoder[BusConnectionSample, String] with

    def apply(bcs: BusConnectionSample): CsvRow[String] =
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

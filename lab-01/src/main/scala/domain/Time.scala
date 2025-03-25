package domain

import cats.implicits.*
import cats.kernel.Eq
import cats.syntax.either.*
import fs2.data.csv.*
import fs2.data.csv.generic.semiauto.*

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

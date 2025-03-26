package domain

import cats.syntax.either.*
import fs2.data.csv.*
import fs2.data.csv.generic.semiauto.*

case class Time(hour: Int, minute: Int, second: Int) extends Product, Serializable {
  def toSeconds: Int = hour * 3600 + minute * 60 + second

  def toMinutes: Int = hour * 60 + minute

  def +(mins: Int): Time = {
    val total = toMinutes + mins
    Time(hour = total / 60 % 24, minute = total % 60, second = 0)
  }

  def to(that: Time): Int =
    if (this.toSeconds > that.toSeconds) 24 * 3600 - this.toSeconds + that.toSeconds
    else that.toSeconds - this.toSeconds

  override def toString: String = f"$hour%02d:$minute%02d:$second%02d"
}

object Time {

  def parse(str: String): Either[String, Time] = {
    def extract(h: String, m: String, s: String) =
      for {
        hour   <- h.toIntOption.toRight(s"Invalid hour: $h").map(_ % 24)
        minute <- m.toIntOption.toRight(s"Invalid minute: $m")
        second <- s.toIntOption.toRight(s"Invalid second: $s")
        _      <- Either.cond(minute >= 0 && minute < 60, (), s"Minute out of range: $str")
        _      <- Either.cond(second >= 0 && second < 60, (), s"Second out of range: $str")
      } yield Time(hour, minute, second)

    str.split(":") match {
      case Array(h, m, s) => extract(h, m, s)
      case Array(h, m)    => extract(h, m, "0")
      case _              => Left(s"Invalid time format: $str")
    }
  }

  implicit val cellDecoder: CellDecoder[Time] = CellDecoder.stringDecoder.emap { str =>
    parse(str).leftMap(DecoderError(_))
  }

}

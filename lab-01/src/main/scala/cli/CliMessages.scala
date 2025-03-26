package cli

import algorithms.utils.Optimization
import cats.implicits.catsSyntaxEq
import domain.{Connection, Stop, Time}

import java.time.LocalTime

object CliMessages {

  def startMsg(startStop: String, destination: String | List[String], optimization: Optimization, startTime: Time): String = {
    val optimizationShow = optimization match {
      case Optimization.Time      => "czas"
      case Optimization.Transfers => "liczba przesiadek"
      case Optimization.Combined  => "czas i liczba przesiadek"
    }

    val destinationMsg = destination match {
      case stop: String => s"do $stop"
      case stops: List[String] => s"przez ${stops.mkString(", ")}"
    }

    s"""Planowanie podróży z $startStop $destinationMsg
     |Kryterium optymalizacji: $optimizationShow
     |Czas startu: $startTime""".stripMargin
  }

  def pathMessage(path: List[Connection]): String =
    s"""Znaleziono trasę:
       |${path
        .foldLeft(List.empty[Connection]) {
          case (Nil, sample)                        => List(sample)
          case (connections @ head :: tail, sample) =>
            if (head.company === sample.company && head.line === sample.line)
              head.copy(arrivalTime = sample.arrivalTime, endStop = sample.endStop) :: tail
            else sample :: connections
        }
        .reverse
        .map(vertex => s"${vertex.line}: ${vertex.departureTime} ${vertex.startStop} -> ${vertex.arrivalTime} ${vertex.endStop}")
        .mkString("\n")}
       |""".stripMargin

  def resultDataMessage(cost: Double, duration: Long) =
    s"""Koszt: $cost
       |Czas obliczeń: $duration milisekund"""

}

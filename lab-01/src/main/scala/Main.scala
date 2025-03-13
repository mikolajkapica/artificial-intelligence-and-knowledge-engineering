import java.time.LocalTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

import cats.effect._
import cats.syntax.all._
import cats.syntax.either._

import algorithms.AStar
import algorithms.Dijkstra
import com.monovore.decline._
import com.monovore.decline.effect._
import domain.Stop
import fs2.io.file.Path
import preprocessing.CsvToGraph.getCachedGraphOrReadAndCache

object Main
  extends CommandIOApp(
    name = "shortest-connection",
    header = "Find shortest connections from bus stop A to B",
  ) {

  private val startStopOpt: Opts[String] = Opts.option[String](
    long = "start",
    short = "s",
    metavar = "stop",
    help = "Starting stop",
  )

  private val endStopOpt: Opts[String] = Opts.option[String](
    long = "end",
    short = "e",
    metavar = "stop",
    help = "Ending stop",
  )

  private val optimizationOpt: Opts[Char] = Opts.option[Char](
    long = "optimize",
    short = "o",
    metavar = "t lub p",
    help = "Optimization criteria: t (time), p (transfers)", // TODO: Can i do a different letter?
  )

  private val startTimeOpt: Opts[LocalTime] = Opts
    .option[String](
      long = "time",
      short = "t",
      metavar = "time",
      help = "Arrival time at the start stop (HH:mm)",
    )
    .mapValidated { timeStr =>
      Either
        .catchNonFatal(LocalTime.parse(timeStr, DateTimeFormatter.ofPattern("HH:mm")))
        .left
        .map(ex => s"Incorrect time format: ${ex.getMessage}")
        .toValidatedNel
    }

  val data = "./data/connection_graph.csv"
  val cacheDir = "./cache"
  val graphFileName = "graph.bin"
  val cachePath = Path(cacheDir) / graphFileName

  override def main: Opts[IO[ExitCode]] = (startStopOpt, endStopOpt, optimizationOpt, startTimeOpt)
    .mapN { (startStop, endStop, optimization, startTime) =>
      println(s"Planowanie podróży z $startStop do $endStop")
      println(s"Kryterium optymalizacji: ${if (optimization === 't') "czas" else "przesiadki"}")
      println(s"Czas startu: $startTime")
      getCachedGraphOrReadAndCache(data, cachePath).flatMap { graph =>
        IO {

          val start = LocalTime.now()

          val algorithm = "A*"

          (
            graph.keys.find(_.name === startStop),
            graph.keys.find(_.name === endStop),
          ).flatMapN { (start, end) =>
            algorithm match {
              case "A*"       =>
                AStar.aStar[Stop](
                  start = start,
                  end = end,
                  neighbors = graph.apply.andThen(_.map(_.endStop)),
                  cost = (a, b) => graph(a).find(_.endStop === b).map(_.arrivalTime.hour.toDouble).getOrElse(Double.MaxValue),
                  heuristic = (a, b) => b.coordinates.latitude - a.coordinates.latitude + b.coordinates.longitude - a.coordinates.longitude,
                )
              case "Dijkstra" =>
                Dijkstra
                  .dijkstra[Stop](
                    start = start,
                    end = end,
                    vertices = graph.keys.toSet,
                    w = (a, b) => graph(a).find(_.endStop === b).map(_.arrivalTime.hour.toDouble).getOrElse(Double.MaxValue),
                  )
                  .some
            }
          }.foreach { path =>
            println("Znaleziono trasę:")
            println(path)
          }

          val end = LocalTime.now()
          println(s"Koniec: $end")

          // TODO:
          // calculate
          // lst.foreach(_.foreach(System.err.println))
          // timeit ?
          println(s"Czas trwania: ${start.until(end, ChronoUnit.MILLIS)} milisekund")
          // cost ?

          ExitCode.Success
        }
      }

    }

}

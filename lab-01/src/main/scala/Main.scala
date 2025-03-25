import algorithms.PathFindingAlgorithm.AStar
import algorithms.PathFindingAlgorithm.Dijkstra
import algorithms.Optimization
import algorithms.PathFindingAlgorithm
import algorithms.PathFindingResult
import algorithms.cost
import algorithms.findShortestPath
import cats.effect.*
import cats.syntax.all.*
import cats.syntax.either.*
import com.monovore.decline.*
import com.monovore.decline.effect.*
import domain.Connection
import domain.Graph
import domain.Stop
import fs2.io.file.Path
import preprocessing.CsvToGraph.getCachedGraphOrReadAndCache

import java.time.LocalTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

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

  private val optimizationOpt: Opts[Optimization] = Opts
    .option[Char](
      long = "optimize",
      short = "o",
      metavar = "t lub p",
      help = "Optimization criteria: t (time), p (transfers)",
    )
    .mapValidated {
      case 't' => Optimization.Time.validNel
      case 'p' => Optimization.Transfers.validNel
      case str => s"Unknown optimization criteria: $str".invalidNel
    }

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
        .map(ex => s"Incorrect time format. Expected \"HH:mm\". ${ex.getMessage}")
        .toValidatedNel
    }

  private val algorithmOpt: Opts[PathFindingAlgorithm] = Opts
    .option[String](
      long = "algorithm",
      short = "a",
      metavar = "algorithm",
      help = "Algorithm to use: AStar (a) or Dijkstra (d)",
    )
    .withDefault("a")
    .mapValidated {
      case "a" => AStar.validNel
      case "d" => Dijkstra.validNel
      case str => s"Unknown algorithm: $str".invalidNel
    }

  private val Data = "./data/connection_graph.csv"
  private val CacheDir = "./.cache"
  private val GraphFileName = "graph.bin"
  private val CachePath = Path(CacheDir) / GraphFileName

  override def main: Opts[IO[ExitCode]] = (startStopOpt, endStopOpt, optimizationOpt, startTimeOpt, algorithmOpt)
    .mapN { (startStop, endStop, optimization, startTime, algorithm) =>

      def startMsg: String = {
        val optimizationShow = optimization match {
          case Optimization.Time      => "czas"
          case Optimization.Transfers => "liczba przesiadek"
        }
        s"""Planowanie podróży z $startStop do $endStop
           |Kryterium optymalizacji: $optimizationShow
           |Czas startu: $startTime""".stripMargin
      }

      def run(graph: Graph, cost: Graph => (Stop, Stop) => Double): Option[PathFindingResult] =
        (
          graph.keys.find(_.name === startStop),
          graph.keys.find(_.name === endStop),
        ).flatMapN { (start, end) =>
          findShortestPath(algorithm, graph, start, end, cost(graph))
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
            .map(vertex => s"${vertex.line}: ${vertex.departureTime} ${vertex.startStop} -> ${vertex.arrivalTime} ${vertex.endStop}")
            .mkString("\n")}
             |""".stripMargin

      def resultDataMessage(cost: Double, duration: Long) =
        s"""Koszt: $cost
           |Czas obliczeń: $duration milisekund"""

      val program = for {
        _      <- IO.println(startMsg)
        graph  <- getCachedGraphOrReadAndCache(Data, CachePath)
        startTime = LocalTime.now()
        result <- run(graph, cost(optimization)).fold(IO.raiseError(new Exception("Nie znaleziono trasy")))(_.pure[IO])
        PathFindingResult(path, cost) = result
        endTime = LocalTime.now()
        _      <- IO.println(pathMessage(path))
        durationMillis = startTime.until(endTime, ChronoUnit.MILLIS)
        _      <- IO(System.err.println(resultDataMessage(cost, durationMillis)))
      } yield ExitCode.Success

      program.handleErrorWith { ex =>
        IO(System.err.println(s"Error: ${ex.getMessage}")).as(ExitCode.Error)
      }
    }

}

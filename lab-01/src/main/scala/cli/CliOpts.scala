package cli

import algorithms.MultipleStopsPathFindingAlgorithm.TabuSearch
import algorithms.MultipleStopsPathFindingAlgorithm
import algorithms.Optimization
import algorithms.SingleEndStopPathFindingAlgorithm
import algorithms.SingleEndStopPathFindingAlgorithm.AStar
import algorithms.SingleEndStopPathFindingAlgorithm.Dijkstra
import cats.syntax.all.*
import cli.CliOpts.ProgramConfig.MultipleStopsConfig
import cli.CliOpts.ProgramConfig.SingleEndStopConfig
import com.monovore.decline.Opts

import java.time.LocalTime
import java.time.format.DateTimeFormatter

object CliOpts {

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

  private val stopsToVisitOpt: Opts[List[String]] = Opts
    .option[String](
      long = "stops",
      short = "v",
      metavar = "stops",
      help = "Stops to visit, separated by semicolons",
    )
    .map(_.split(";").toList)

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

  private val singleEndStopPathfindingAlgorithmOpt: Opts[SingleEndStopPathFindingAlgorithm] = Opts
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

  private val multipleStopsPathfindingAlgorithmOpt: Opts[MultipleStopsPathFindingAlgorithm] = Opts
    .option[String](
      long = "algorithm",
      short = "a",
      metavar = "algorithm",
      help = "Algorithm to use: TabuSearch (t)",
    )
    .withDefault("t")
    .mapValidated {
      case "t" => TabuSearch.validNel
      case str => s"Unknown algorithm: $str".invalidNel
    }

  sealed trait ProgramConfig

  object ProgramConfig:

    case class SingleEndStopConfig(
                                    startStop: String,
                                    endStop: String,
                                    optimization: Optimization,
                                    startTime: LocalTime,
                                    algorithm: SingleEndStopPathFindingAlgorithm,
    ) extends ProgramConfig

    case class MultipleStopsConfig(
                                    startStop: String,
                                    stopsToVisit: List[String],
                                    optimization: Optimization,
                                    startTime: LocalTime,
                                    algorithm: MultipleStopsPathFindingAlgorithm,
    ) extends ProgramConfig

  private val singleEndStopOpts: Opts[SingleEndStopConfig] =
    (startStopOpt, endStopOpt, optimizationOpt, startTimeOpt, singleEndStopPathfindingAlgorithmOpt).mapN(
      (startStop, endStop, optimization, startTime, algorithm) =>
        SingleEndStopConfig(startStop, endStop, optimization, startTime, algorithm)
    )

  private val multipleOpts: Opts[MultipleStopsConfig] =
    (startStopOpt, stopsToVisitOpt, optimizationOpt, startTimeOpt, multipleStopsPathfindingAlgorithmOpt)
      .mapN((startStop, stopsToVisit, optimization, startTime, algorithm) =>
        MultipleStopsConfig(startStop, stopsToVisit, optimization, startTime, algorithm)
      )

  val mainOpts: Opts[ProgramConfig] = singleEndStopOpts.orElse(multipleOpts)
}

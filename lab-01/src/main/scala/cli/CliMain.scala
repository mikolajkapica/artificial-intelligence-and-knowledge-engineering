package cli

import algorithms.*
import cats.effect.*
import cats.syntax.all.*
import cli.CliOpts.*
import cli.CliMessages.*
import com.monovore.decline.*
import com.monovore.decline.effect.*
import domain.Stop
import fs2.io.file.Path
import ProgramConfig.*
import algorithms.Heuristics.Heuristic
import algorithms.Heuristics.getHeuristic
import algorithms.utils.CostFunctions.getCostFunction
import algorithms.utils.NoPathFoundException
import algorithms.utils.PathFindingResult
import algorithms.utils.findShortestPath
import preprocessing.CsvToGraph.getCachedGraphOrReadAndCache

import java.time.LocalTime
import java.time.temporal.ChronoUnit
import scala.util.Random

object CliMain
  extends CommandIOApp(
    name = "shortest-connection",
    header = "Find shortest connections from bus stop A to B",
  ) {

  // constants
  private val Data = "./data/connection_graph.csv"
  private val CacheDir = "./.cache"
  private val GraphFileName = "graph.bin"
  private val CachePath = Path(CacheDir) / GraphFileName
  private val heuristicType = Heuristic.EuclideanDegree
  Random.setSeed(42)

  override def main: Opts[IO[ExitCode]] =
    mainOpts
      .map {
        case config: SingleEndStopConfig =>
          for {
            graph                 <- getCachedGraphOrReadAndCache(Data, CachePath)
            startStopParsed: Stop <- Stop.parseIO(config.startStop, graph)
            endStopParsed         <- Stop.parseIO(config.endStop, graph)
            costFunction = getCostFunction(config.optimization)
            heuristic = getHeuristic(heuristicType)
            _                     <- IO.println {
                                       startMsg(
                                         config.startStop,
                                         config.endStop,
                                         config.optimization,
                                         config.startTime,
                                       )
                                     }
            startTime = LocalTime.now()
            result                <- IO.fromOption(
                                       findShortestPath(
                                         config.algorithm,
                                         graph,
                                         startStopParsed,
                                         config.startTime,
                                         endStopParsed,
                                         costFunction,
                                         heuristic,
                                       )
                                     )(new NoPathFoundException)
            PathFindingResult(path, cost) = result
            endTime = LocalTime.now()
            _                     <- IO.println(pathMessage(path))
            durationMillis = startTime.until(endTime, ChronoUnit.MILLIS)
            _                     <- IO(System.err.println(resultDataMessage(cost, durationMillis)))
          } yield ExitCode.Success
        case config: MultipleStopsConfig =>
          for {
            graph                 <- getCachedGraphOrReadAndCache(Data, CachePath)
            startStopParsed: Stop <- Stop.parseIO(config.startStop, graph)
            endStopParsed         <- config.stopsToVisit.traverse(Stop.parseIO(_, graph))
            costFunction = getCostFunction(config.optimization)
            heuristic = getHeuristic(heuristicType)
            _                     <- IO.println {
                                       startMsg(
                                         config.startStop,
                                         config.stopsToVisit,
                                         config.optimization,
                                         config.startTime,
                                       )
                                     }
            startTime = LocalTime.now()
            result                <- IO.fromOption {
                                       findShortestPath(
                                         config.algorithm,
                                         graph,
                                         startStopParsed,
                                         config.startTime,
                                         endStopParsed,
                                         costFunction,
                                         heuristic,
                                       )
                                     }(new NoPathFoundException)
            PathFindingResult(path, cost) = result
            endTime = LocalTime.now()
            _                     <- IO.println(pathMessage(path))
            durationMillis = startTime.until(endTime, ChronoUnit.MILLIS)
            _                     <- IO(System.err.println(resultDataMessage(cost, durationMillis)))
          } yield ExitCode.Success
      }
      .map(_.recover { case e: NoPathFoundException =>
        println(e.getMessage)
        ExitCode.Success
      })

}

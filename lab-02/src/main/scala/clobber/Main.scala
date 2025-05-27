package clobber

import cats.effect._
import cats.syntax.all._
import cats.syntax.option._

import clobber.ai._
import clobber.ai.MinimaxSearch
import java.util.concurrent.TimeUnit
import scala.io.StdIn
import clobber.{getAIConfig, PlayerAIConfig}

object Main extends IOApp.Simple {
  override def run: IO[Unit] = (for {
    configBlack <- getAIConfig(Player.Black, defaultDepth = 3, defaultAlgorithm = Algorithm.Minimax)
    configWhite <- getAIConfig(Player.White, defaultDepth = 3, defaultAlgorithm = Algorithm.AlphaBeta)
    aiConfigs = AiConfigs(configBlack, configWhite)

    _ <- IO.println(
      s"""|
          |Enter initial board configuration (m lines, n space-separated chars, end with an empty line):""".stripMargin
    )
    initialBoard <- readBoardConfig.map(Board.parse).map(_.leftMap(Throwable(_))).flatMap(IO.fromEither)

    nodesVisitedRef <- Ref[IO].of(0L)
    startTime <- Clock[IO].realTime
    initialPlayer = Player.Black

    _ <- IO.println(
      s"""|
          |Starting game. 
          |Black AI: ${configBlack.algorithm}, Depth ${configBlack.depth}, Heuristic ${configBlack.heuristic}
          |White AI: ${configWhite.algorithm}, Depth ${configWhite.depth}, Heuristic ${configWhite.heuristic}
          |""".stripMargin
    )

    gameResult <- gameLoop(initialBoard, initialPlayer, rounds = 0, aiConfigs, nodesVisitedRef)
    (finalBoard, winner, finalRounds) = gameResult

    endTime <- Clock[IO].realTime
    totalVisitedNodes <- nodesVisitedRef.get

    _ <- IO.println(
      s"""|--- Game Over ---
          |Final board:
          |$finalBoard
          |Rounds: $finalRounds
          |Winner: ${winner.shortName}""".stripMargin
    )
    _ <- IO(System.err.println(
      s"""|Visited nodes: $totalVisitedNodes
          |Execution time: ${endTime - startTime}""".stripMargin
    ))
  } yield ()).onError {
    case e: NumberFormatException =>
      IO(System.err.println(s"Input Error: ${e.getMessage}"))
    case e: IllegalArgumentException =>
      IO(System.err.println(s"Configuration Error: ${e.getMessage}"))
    case e: Exception =>
      IO(System.err.println(s"An unexpected error occurred: ${e.getMessage}")) >> IO(e.printStackTrace())
  }
}

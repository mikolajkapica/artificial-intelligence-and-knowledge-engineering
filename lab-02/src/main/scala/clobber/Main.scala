package clobber

import cats.effect.*
import cats.effect.unsafe.IORuntimeConfig
import cats.syntax.all.*
import clobber.ai.{AiConfigs, Algorithm, getAIConfig, readBoardConfig}

import scala.concurrent.duration.Duration

object Main extends IOApp:
  override def runtimeConfig: IORuntimeConfig =
    super.runtimeConfig.copy(cpuStarvationCheckInitialDelay = Duration.Inf)

  override def run(args: List[String]): IO[ExitCode] = (for {
    configBlack <- getAIConfig(Player.Black, defaultDepth = 3, defaultAlgorithm = Algorithm.Minimax)
    configWhite <- getAIConfig(Player.White, defaultDepth = 3, defaultAlgorithm = Algorithm.AlphaBeta)
    aiConfigs = AiConfigs(configBlack, configWhite)

    _ <- IO.println("Enter initial board configuration (m lines, n space-separated chars, end with an empty line):")
    initialBoard <- readBoardConfig.map(Board.parse).map(_.leftMap(Throwable(_))).flatMap(IO.fromEither)

    nodesVisitedRef <- Ref[IO].of(0L)
    startTime <- Clock[IO].realTime
    initialPlayer = Player.Black

    _ <- IO.println(
      s"""Starting game.
         |Black AI: ${configBlack.algorithm}, depth ${configBlack.depth}, heuristic ${configBlack.heuristic}
         |White AI: ${configWhite.algorithm}, depth ${configWhite.depth}, heuristic ${configWhite.heuristic}
         |""".stripMargin
    )

    gameResult <- gameLoop(initialBoard, initialPlayer, rounds = 0, aiConfigs, nodesVisitedRef)
    (finalBoard, winner, finalRounds) = gameResult

    endTime <- Clock[IO].realTime
    totalVisitedNodes <- nodesVisitedRef.get

    _ <- IO.println(
      s"""|
          |--- Game Over ---
          |Final board:
          |$finalBoard
          |Rounds: $finalRounds
          |Winner: ${winner.shortName}
          |""".stripMargin
    )
    _ <- IO(System.err.println(
      f"""|Visited nodes: $totalVisitedNodes
          |Execution time: ${(endTime - startTime).toMillis / 1000.0}%.2f seconds""".stripMargin
    ))
  } yield ExitCode.Success).onError { err =>
    val errIO = err match {
      case e: NumberFormatException =>
        IO(System.err.println(s"Input Error: ${e.getMessage}"))
      case e: IllegalArgumentException =>
        IO(System.err.println(s"Configuration Error: ${e.getMessage}"))
      case e: Exception =>
        IO(System.err.println(s"An unexpected error occurred: ${e.getMessage}")) >> IO(e.printStackTrace())
    }
    errIO >> IO.pure(ExitCode.Error)
  }
end Main

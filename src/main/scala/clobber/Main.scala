package clobber

import cats.effect._
import cats.syntax.all._
import clobber.model._
import clobber.ai._
import clobber.ai.MinimaxSearch // Added import
import clobber.game.MoveGenerator
import java.util.concurrent.TimeUnit
import scala.io.StdIn

// Definitions from the old Main.scala, to be moved or ensure they are in clobber.model
// For now, let's assume they are accessible via clobber.model._
// Ensure other model classes like Board are in their respective files and imported.
// The model classes (Player, Square, Position, Move) have been moved to 
// their respective files in the clobber.model package.

// Define PlayerAIConfig case class
case class PlayerAIConfig(heuristic: Heuristic, depth: Int, algorithm: String)

object Main extends IOApp.Simple {

  def selectHeuristic(name: String): IO[Heuristic] = name.toLowerCase match {
    case "mobility" => IO.pure(MobilityHeuristic)
    case "piececount" => IO.pure(PieceCountHeuristic)
    case "positional" => IO.pure(PositionalHeuristic)
    case _ => IO.raiseError(new IllegalArgumentException(s"Unknown heuristic: $name. Valid options: mobility, piececount, positional."))
  }

  // Helper function to read AI config for a given player
  def getAIConfig(player: Player, defaultDepth: Int = 3, defaultAlgorithm: String = "alphabeta"): IO[PlayerAIConfig] = {
    val playerName = player.toString // "Black" or "White"
    for {
      _ <- IO(println(s"\nConfiguring AI for $playerName:"))
      _ <- IO(println(s"Select heuristic for $playerName (mobility, piececount, positional):"))
      heuristicName <- IO(StdIn.readLine().trim.toLowerCase)
      heuristic <- selectHeuristic(heuristicName)

      _ <- IO(println(s"Enter search depth for $playerName (e.g., $defaultDepth):"))
      depthStr <- IO(StdIn.readLine().trim)
      depth <- IO(depthStr.toIntOption.getOrElse(defaultDepth)) // Handle potential parse error, fallback to default

      _ <- IO(println(s"Select search algorithm for $playerName (minimax or alphabeta, default: $defaultAlgorithm):"))
      algStr <- IO(StdIn.readLine().trim.toLowerCase)
      algorithm <- algStr match {
        case alg @ ("minimax" | "alphabeta") => IO.pure(alg)
        case "" => IO.pure(defaultAlgorithm) // Default if empty
        case other => IO.raiseError(new IllegalArgumentException(s"Unknown algorithm: $other. Choose 'minimax' or 'alphabeta'."))
      }
      _ <- IO(println(s"$playerName AI configured: Heuristic=${heuristicName}, Depth=${depth}, Algorithm=${algorithm}"))
    } yield PlayerAIConfig(heuristic, depth, algorithm)
  }

  def readBoardConfig: IO[List[String]] = IO {
    LazyList.continually(StdIn.readLine()).takeWhile(line => line != null && line.nonEmpty).toList
  }

  def gameLoop(
      currentBoard: Board,
      currentPlayer: Player,
      rounds: Int,
      aiConfigs: Map[Player, PlayerAIConfig], // Pass Map of configs
      nodesVisitedRef: Ref[IO, Long]
  ): IO[(Board, Player, Int)] = {
    IO(println(s"\nCurrent board (Turn: ${currentPlayer.shortName}, Round: $rounds):\n$currentBoard")) >> {
      val possibleMoves = MoveGenerator.generateMoves(currentBoard, currentPlayer)

      if (possibleMoves.isEmpty) {
        IO(println(s"No moves available for ${currentPlayer.shortName}.")) >>
        IO.pure((currentBoard, currentPlayer.other, rounds)) // Winner is the other player
      } else {
        val currentAIConfig = aiConfigs(currentPlayer)
        val depth = currentAIConfig.depth
        val heuristic = currentAIConfig.heuristic
        val chosenAlgorithm = currentAIConfig.algorithm

        IO(println(s"${currentPlayer.shortName} (using ${chosenAlgorithm}, depth ${depth}, heuristic ${heuristic.getClass.getSimpleName}) is thinking...")) >> {
          val searchResultIO: IO[(Option[Move], Double, Long)] =
            if (chosenAlgorithm == "minimax") {
              IO {
                MinimaxSearch.minimax(
                  currentBoard, depth, currentPlayer, currentPlayer, heuristic
                )
              }
            } else { // alphabeta
              IO {
                AlphaBetaSearch.alphaBeta(
                  currentBoard, depth, Double.NegativeInfinity, Double.PositiveInfinity,
                  currentPlayer, currentPlayer, heuristic
                )
              }
            }

          searchResultIO.flatMap { case (bestMoveOpt, score, nodesInTurn) =>
            nodesVisitedRef.update(_ + nodesInTurn).flatMap { _ =>
              bestMoveOpt match {
                case Some(bestMove) =>
                  IO(println(s"${currentPlayer.shortName} chooses move: $bestMove with score: $score (nodes: $nodesInTurn)")) >> {
                    val nextBoard = currentBoard.applyMove(bestMove, currentPlayer)
                    gameLoop(nextBoard, currentPlayer.other, rounds + 1, aiConfigs, nodesVisitedRef) // Pass aiConfigs
                  }
                case None =>
                  // This case implies AI failed to find a move even if possibleMoves was non-empty.
                  IO.raiseError(new IllegalStateException(s"AI ($chosenAlgorithm) for ${currentPlayer.shortName} failed to find a move but moves were possible."))
            }
          }
        }
      }
    }
  }

  override def run: IO[Unit] = {
    for {
      // Read AI configurations for Black and White
      configBlack <- getAIConfig(Player.Black)
      configWhite <- getAIConfig(Player.White, defaultDepth = 3, defaultAlgorithm = "alphabeta") // Example: different defaults for White
      
      aiConfigs = Map(Player.Black -> configBlack, Player.White -> configWhite)

      _ <- IO(println("\nEnter initial board configuration (m lines, n space-separated chars, end with an empty line):"))
      boardConfigLines <- readBoardConfig
      parsedBoardEither = Board.fromString(boardConfigLines)
      initialBoard <- parsedBoardEither match {
        case Right(board) => IO.pure(board)
        case Left(error)  => IO.raiseError(new IllegalArgumentException(s"Board parsing failed: $error"))
      }

      nodesVisitedRef <- Ref[IO].of(0L)
      startTime <- IO(System.nanoTime())
      
      initialPlayer = Player.Black // Black always starts

      _ <- IO(println(s"\nStarting game. Black AI: ${configBlack.algorithm}, Depth ${configBlack.depth}, Heuristic ${configBlack.heuristic.getClass.getSimpleName}"))
      _ <- IO(println(s"White AI: ${configWhite.algorithm}, Depth ${configWhite.depth}, Heuristic ${configWhite.heuristic.getClass.getSimpleName}"))

      gameResult <- gameLoop(initialBoard, initialPlayer, 0, aiConfigs, nodesVisitedRef) // Pass aiConfigs map
      (finalBoard, winner, finalRounds) = gameResult
      
      endTime <- IO(System.nanoTime())
      durationNanos = endTime - startTime
      durationMillis = TimeUnit.NANOSECONDS.toMillis(durationNanos)

      totalVisitedNodes <- nodesVisitedRef.get

      _ <- IO(println(s"\n--- Game Over ---"))
      _ <- IO(println(s"Final board:\n$finalBoard"))
      _ <- IO(println(s"Rounds: $finalRounds"))
      _ <- IO(println(s"Winner: ${winner.shortName}"))
      
      statsMessage = s"\nVisited nodes: $totalVisitedNodes\nExecution time: ${durationMillis / 1000.0} seconds (${durationMillis} ms)"
      _ <- IO(System.err.println(statsMessage))

    } yield ()
  }.handleErrorWith {
    case e: IllegalArgumentException => IO(System.err.println(s"Configuration Error: ${e.getMessage}"))
    case e: NumberFormatException => IO(System.err.println(s"Input Error: ${e.getMessage}"))
    case e: Exception => IO(System.err.println(s"An unexpected error occurred: ${e.getMessage}")) >> IO(e.printStackTrace())
  }
}

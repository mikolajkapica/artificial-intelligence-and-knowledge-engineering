package clobber.tournaments

import cats.effect.{IO, IOApp, Ref}
import cats.syntax.all.*
import clobber.ai.*
import clobber.{Board, Player, Square, gameLoop}

val heuristics: List[Heuristic] =
  List(MobilityHeuristic, PieceCountHeuristic, PositionalHeuristic, NewTrends, OpponentPenalty, GroupingLonely)
val depths: List[Int] = List(1, 2, 3)
val boardSizes: List[(Int, Int)] = List((5, 5), (5, 8), (10, 10))

def generateBoards: List[Board] = for {
  (width, height) <- boardSizes
  grid = Vector.tabulate(height, width)((row, col) => if ((row + col) % 2 == 0) Square.B else Square.W)
} yield Board(grid, height, width)

def generateAiConfigs: List[AiConfigs] = for {
  heuristic1 <- heuristics
  heuristic2 <- heuristics
  depth1 <- depths
  depth2 <- depths
  player1 = PlayerAIConfig(heuristic1, depth1, Algorithm.AlphaBeta)
  player2 = PlayerAIConfig(heuristic2, depth2, Algorithm.AlphaBeta)
  aiConfigs = AiConfigs(black = player1, white = player2)
} yield aiConfigs

final case class TestResult(board: Board, aiConfigs: AiConfigs, winner: Player, rounds: Int)

def test(board: Board, aiConfigs: AiConfigs): IO[TestResult] = for {
  nodesVisited <- Ref[IO].of(0L)
  result <- gameLoop(board, Player.Black, 0, aiConfigs, nodesVisited)
  (_, winner, rounds) = result
  nodesVisitedCount <- nodesVisited.get
} yield TestResult(board, aiConfigs, winner, rounds)

object Main extends IOApp.Simple:
  override def run: IO[Unit] =
    val configs = for {
      board <- generateBoards
      aiConfigs <- generateAiConfigs
    } yield (board, aiConfigs)
    
    val tests = configs.zipWithIndex.parTraverse { case ((board, aiConfigs), i) =>
      test(board, aiConfigs) <* IO.println(s"Test $i/${configs.size}")
    }

    tests >>= { results =>
      val winners = results.groupBy(_.winner).view.mapValues(_.size).toMap

      val heuristicWins = results.flatMap { result =>
        val blackHeuristic = result.aiConfigs.black.heuristic.toString
        val whiteHeuristic = result.aiConfigs.white.heuristic.toString
        if (result.winner == Player.Black) Some(blackHeuristic) else Some(whiteHeuristic)
      }.groupBy(identity).view.mapValues(_.size).toMap

      IO.println(
        s"""Tournament Results:
           |Winners by player: $winners
           |Heuristic wins: $heuristicWins
           |Total tests run: ${results.size}""".stripMargin
      )
    }

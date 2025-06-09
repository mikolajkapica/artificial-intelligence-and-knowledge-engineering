package clobber

import cats.effect.IO
import cats.effect.kernel.Ref
import clobber.ai.*

def generateMoves(board: Board, player: Player): List[Move] = (for {
  row <- 0 until board.numRows
  column <- 0 until board.numCols
  currentPos = Position(row, column)
  square <- board.getSquare(currentPos).toList
  if (player == Player.Black && square == Square.B)
    || (player == Player.White && square == Square.W)
  targetPosition <- List(
    Position(row + 1, column),
    Position(row - 1, column),
    Position(row, column + 1),
    Position(row, column - 1)
  )
  targetSquare <- board.getSquare(targetPosition)
  opponentPiece = player.other match {
    case Player.Black => Square.B
    case Player.White => Square.W
  }
  if targetSquare == opponentPiece
} yield Move(currentPos, targetPosition)).toList

def gameLoop(
    currentBoard: Board,
    currentPlayer: Player,
    rounds: Int,
    aiConfigs: AiConfigs,
    nodesVisitedRef: Ref[IO, Long]
): IO[(Board, Player, Int)] = {
  val PlayerAIConfig(heuristic, depth, algorithm) = aiConfigs.get(currentPlayer)

  val search: IO[(Option[Move], Double, Long)] = algorithm match {
    case Algorithm.Minimax =>
      IO(MinimaxSearch.minimax(
        currentBoard,
        depth,
        currentPlayer,
        currentPlayer,
        heuristic
      ))
    case Algorithm.AlphaBeta =>
      IO(AlphaBetaSearch.alphaBeta(
        currentBoard,
        depth,
        Double.NegativeInfinity,
        Double.PositiveInfinity,
        currentPlayer,
        currentPlayer,
        heuristic
      ))
  }

  for {
    _ <- IO.println(
      s"""|
          |Current board (Turn: ${currentPlayer.shortName}, Round: $rounds):
          |$currentBoard
          |${currentPlayer.shortName} (using $algorithm, depth $depth, heuristic $heuristic) is thinking...""".stripMargin
    )
    searchResult <- search
    (bestMoveOpt, score, nodesInTurn) = searchResult
    _ <- nodesVisitedRef.update(_ + nodesInTurn)

    result <- bestMoveOpt match {
      case None =>
        IO.println(s"${currentPlayer.shortName} has no valid moves left.") >>
          IO.pure((currentBoard, currentPlayer.other, rounds + 1))
      case Some(bestMove) => for {
          _ <-
            IO.println(s"${currentPlayer.shortName} chooses move: $bestMove with score: $score (nodes: $nodesInTurn)")
          nextBoard = currentBoard.applyMove(bestMove, currentPlayer)
          result <- gameLoop(nextBoard, currentPlayer.other, rounds + 1, aiConfigs, nodesVisitedRef)
        } yield result
    }
  } yield result
}

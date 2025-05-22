package clobber.game

import clobber.model.{Board, Move, Player, Position, Square}

object MoveGenerator {

  def generateMoves(board: Board, player: Player): List[Move] = {
    val moves = for {
      r <- 0 until board.numRows
      c <- 0 until board.numCols
      currentPos = Position(r, c)
      square <- board.getSquare(currentPos)
      if (player == Player.Black && square == Square.B) || (player == Player.White && square == Square.W)
    } yield {
      val potentialTargets = List(
        Position(r + 1, c), // down
        Position(r - 1, c), // up
        Position(r, c + 1), // right
        Position(r, c - 1)  // left
      )

      potentialTargets.flatMap { targetPos =>
        if (board.isOnBoard(targetPos)) {
          board.getSquare(targetPos) match {
            case Some(targetSquare) =>
              val opponentPiece = player.other match {
                case Player.Black => Square.B
                case Player.White => Square.W
              }
              if (targetSquare == opponentPiece) {
                Some(Move(currentPos, targetPos))
              } else {
                None
              }
            case None => None // Should not happen if isOnBoard is true and getSquare is consistent
          }
        } else {
          None
        }
      }
    }
    moves.flatten.toList
  }
}

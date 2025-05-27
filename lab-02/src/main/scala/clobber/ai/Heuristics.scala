package clobber.ai

import clobber.{Board, Player, Square}
import clobber.generateMoves

trait Heuristic {
  /**
   * Evaluates the board state from the perspective of the `forPlayer`.
   * A higher score is better for the `forPlayer`.
   *
   * @param board The current game board.
   * @param forPlayer The player for whom the evaluation is being made.
   * @return The heuristic value of the board state.
   */
  def evaluate(board: Board, forPlayer: Player): Double

  override def toString: String = this match {
    case MobilityHeuristic   => "mobility"
    case PieceCountHeuristic => "piececount"
    case PositionalHeuristic => "positional"
  }
}

object Heuristic {
  def parse(name: String): Option[Heuristic] = name.toLowerCase match {
    case "mobility"   => Some(MobilityHeuristic)
    case "piececount" => Some(PieceCountHeuristic)
    case "positional" => Some(PositionalHeuristic)
    case _            => None
  }
}

object MobilityHeuristic extends Heuristic {
  override def evaluate(board: Board, forPlayer: Player): Double = {
    val myMoves = generateMoves(board, forPlayer).size.toDouble
    val opponentMoves = generateMoves(board, forPlayer.other).size.toDouble
    myMoves - opponentMoves
  }
}

object PieceCountHeuristic extends Heuristic {
  override def evaluate(board: Board, forPlayer: Player): Double = {
    var myPieceCount = 0
    var opponentPieceCount = 0

    val myPlayerSquare = if (forPlayer == Player.Black) Square.B else Square.W
    val opponentPlayerSquare = if (forPlayer.other == Player.Black) Square.B else Square.W

    for (r <- 0 until board.numRows) {
      for (c <- 0 until board.numCols) {
        board.grid(r)(c) match {
          case s if s == myPlayerSquare => myPieceCount += 1
          case s if s == opponentPlayerSquare => opponentPieceCount += 1
          case _ => // Empty square
        }
      }
    }
    (myPieceCount - opponentPieceCount).toDouble
  }
}

object PositionalHeuristic extends Heuristic {
  override def evaluate(board: Board, forPlayer: Player): Double = {
    var myPositionalScore = 0.0
    var opponentPositionalScore = 0.0

    val myPlayerSquare = if (forPlayer == Player.Black) Square.B else Square.W
    val opponentPlayerSquare = if (forPlayer.other == Player.Black) Square.B else Square.W

    for (r <- 0 until board.numRows) {
      for (c <- 0 until board.numCols) {
        val score = getPositionScore(r, c, board.numRows, board.numCols)
        board.grid(r)(c) match {
          case s if s == myPlayerSquare => myPositionalScore += score
          case s if s == opponentPlayerSquare => opponentPositionalScore += score
          case _ => // Empty square
        }
      }
    }
    myPositionalScore - opponentPositionalScore
  }

  private def getPositionScore(row: Int, col: Int, numRows: Int, numCols: Int): Double = {
    val isCorner = (row == 0 || row == numRows - 1) && (col == 0 || col == numCols - 1)
    val isEdge = (row == 0 || row == numRows - 1 || col == 0 || col == numCols - 1) && !isCorner

    if (isCorner) {
      0.5 // Corner piece
    } else if (isEdge) {
      1.0 // Edge piece
    } else {
      2.0 // Center piece
    }
  }
}

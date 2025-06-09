package clobber.ai

import clobber.{Board, Move, Player}
import clobber.generateMoves

object AlphaBetaSearch {
  def alphaBeta(
      board: Board,
      depth: Int,
      alpha: Double,
      beta: Double,
      currentPlayer: Player,
      maximizingPlayer: Player,
      heuristic: Heuristic
  ): (Option[Move], Double, Long) = {

    var nodesThisCall: Long = 1L // Count current node evaluation

    val possibleMoves = generateMoves(board, currentPlayer)

    if (depth == 0 || possibleMoves.isEmpty) {
      return (None, heuristic.evaluate(board, maximizingPlayer), nodesThisCall)
    }

    var bestMove: Option[Move] = None

    var currentAlpha = alpha
    var currentBeta = beta

    if (currentPlayer == maximizingPlayer) { // Maximizing player's turn
      var maxEval = Double.NegativeInfinity
      val movesIterator = possibleMoves.iterator

      while (movesIterator.hasNext) {
        val move = movesIterator.next()
        val nextBoard = board.applyMove(move, currentPlayer)

        val (_, eval, nodesFromRec) =
          alphaBeta(nextBoard, depth - 1, currentAlpha, currentBeta, currentPlayer.other, maximizingPlayer, heuristic)
        nodesThisCall += nodesFromRec

        if (eval > maxEval) {
          maxEval = eval
          bestMove = Some(move)
        }
        currentAlpha = Math.max(currentAlpha, eval)
        if (currentBeta <= currentAlpha) {
          return (bestMove, maxEval, nodesThisCall)
        }
      }
      (bestMove, maxEval, nodesThisCall)
    } else {
      var minEval = Double.PositiveInfinity
      val movesIterator = possibleMoves.iterator

      while (movesIterator.hasNext) {
        val move = movesIterator.next()
        val nextBoard = board.applyMove(move, currentPlayer)

        val (_, eval, nodesFromRec) =
          alphaBeta(nextBoard, depth - 1, currentAlpha, currentBeta, currentPlayer.other, maximizingPlayer, heuristic)
        nodesThisCall += nodesFromRec

        if (eval < minEval) {
          minEval = eval
          bestMove = Some(move)
        }
        currentBeta = Math.min(currentBeta, eval)
        if (currentBeta <= currentAlpha) {
          return (bestMove, minEval, nodesThisCall)
        }
      }
      (bestMove, minEval, nodesThisCall)
    }
  }
}

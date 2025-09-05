package clobber.ai

import clobber.{Board, Move, Player, generateMoves}

object MinimaxSearch {

  def minimax(
      board: Board,
      depth: Int,
      currentPlayer: Player,
      maximizingPlayer: Player,
      heuristic: Heuristic
  ): (Option[Move], Double, Long) = {

    var nodesVisitedThisCall: Long = 1L

    val possibleMoves = generateMoves(board, currentPlayer)

    if (depth == 0 || possibleMoves.isEmpty) {
      return (None, heuristic.evaluate(board, maximizingPlayer), nodesVisitedThisCall)
    }

    var bestMove: Option[Move] = possibleMoves.headOption

    if (currentPlayer == maximizingPlayer) {
      var maxEval = Double.NegativeInfinity

      for (move <- possibleMoves) {
        val nextBoard = board.applyMove(move, currentPlayer)

        val (_, eval, nodesFromRec) = minimax(nextBoard, depth - 1, currentPlayer.other, maximizingPlayer, heuristic)
        nodesVisitedThisCall += nodesFromRec

        if (eval > maxEval) {
          maxEval = eval
          bestMove = Some(move)
        }
      }

      (bestMove, maxEval, nodesVisitedThisCall)
    } else {
      var minEval = Double.PositiveInfinity

      for (move <- possibleMoves) {
        val nextBoard = board.applyMove(move, currentPlayer)

        val (_, eval, nodesFromRec) = minimax(nextBoard, depth - 1, currentPlayer.other, maximizingPlayer, heuristic)
        nodesVisitedThisCall += nodesFromRec

        if (eval < minEval) {
          minEval = eval
          bestMove = Some(move)
        }
      }
      (bestMove, minEval, nodesVisitedThisCall)
    }
  }
}

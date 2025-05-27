package clobber.ai

import clobber.{Board, Move, Player}
import clobber.generateMoves
// Heuristic trait is also needed for the function signature
import clobber.ai.Heuristic

object MinimaxSearch {

  /**
   * Performs a classic Minimax search to find the best move.
   *
   * @param board            The current board state.
   * @param depth            The maximum search depth.
   * @param currentPlayer      The player whose turn it is to move in the current state.
   * @param maximizingPlayer The player whose perspective the search is trying to maximize.
   * @param heuristic        The heuristic function to evaluate non-terminal states or states at max depth.
   * @return A tuple containing the best Option[Move], its score (Double), and nodes visited (Long).
   */
  def minimax(
      board: Board,
      depth: Int,
      currentPlayer: Player,
      maximizingPlayer: Player,
      heuristic: Heuristic
  ): (Option[Move], Double, Long) = {

    var nodesVisitedThisCall: Long = 1L // Count current node

    val possibleMoves = generateMoves(board, currentPlayer)

    // Base Case: Depth limit reached or game over for currentPlayer
    if (depth == 0 || possibleMoves.isEmpty) {
      return (None, heuristic.evaluate(board, maximizingPlayer), nodesVisitedThisCall)
    }

    var bestMove: Option[Move] = None // Will be Some(move) if possibleMoves is not empty

    if (currentPlayer == maximizingPlayer) { // Maximizing player's turn
      var maxEval = Double.NegativeInfinity
      
      // Initialize bestMove with the first move to ensure it's set if moves are possible
      // This also handles the case where all subsequent moves might have worse or equal scores.
      bestMove = possibleMoves.headOption 

      for (move <- possibleMoves) {
        val nextBoard = board.applyMove(move, currentPlayer)
        // Recursive call for the other player (minimizer)
        val (_, eval, nodesFromRec) = minimax(nextBoard, depth - 1, currentPlayer.other, maximizingPlayer, heuristic)
        nodesVisitedThisCall += nodesFromRec
        
        if (eval > maxEval) {
          maxEval = eval
          bestMove = Some(move)
        }
      }
      // If maxEval is still NegativeInfinity (e.g., all moves lead to immediate loss evaluated as -Infinity by heuristic, or possibleMoves was empty),
      // and possibleMoves was not empty, bestMove would be Some(possibleMoves.head).
      // If possibleMoves was not empty, and loop ran, maxEval would be updated from -Infinity
      // unless all evaluations were -Infinity. In that case, bestMove would still point to the move that produced that -Infinity.
      // The initial bestMove = possibleMoves.headOption takes care of setting a default move.
      // If possibleMoves is non-empty, maxEval must be updated by at least the first move's evaluation.
      // If all moves yield -Infinity, maxEval remains -Infinity, and bestMove points to one of those moves.
      // If possibleMoves.nonEmpty, bestMove *will* be Some(move).
      // The only way maxEval remains -Infinity is if heuristic returns -Infinity for all children.
      (bestMove, maxEval, nodesVisitedThisCall)
    } else { // Minimizing player's turn
      var minEval = Double.PositiveInfinity
      
      bestMove = possibleMoves.headOption

      for (move <- possibleMoves) {
        val nextBoard = board.applyMove(move, currentPlayer)
        // Recursive call for the other player (maximizer)
        val (_, eval, nodesFromRec) = minimax(nextBoard, depth - 1, currentPlayer.other, maximizingPlayer, heuristic)
        nodesVisitedThisCall += nodesFromRec

        if (eval < minEval) {
          minEval = eval
          bestMove = Some(move)
        }
      }
      // Similar logic to maximizer: if possibleMoves.nonEmpty, bestMove will be Some(move).
      // minEval will be updated unless all evaluations are +Infinity.
      (bestMove, minEval, nodesVisitedThisCall)
    }
  }
}

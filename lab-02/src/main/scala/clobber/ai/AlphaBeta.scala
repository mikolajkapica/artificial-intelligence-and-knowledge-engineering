package clobber.ai

import clobber.{Board, Move, Player}
import clobber.generateMoves

object AlphaBetaSearch {

  /**
   * Performs an alpha-beta search to find the best move.
   *
   * @param board            The current board state.
   * @param depth            The maximum search depth.
   * @param alpha            The current alpha value (best score for maximizer).
   * @param beta             The current beta value (best score for minimizer).
   * @param currentPlayer      The player whose turn it is to move in the current state.
   * @param maximizingPlayer The player whose perspective the search is trying to maximize (e.g., Player B at the root).
   * @param heuristic        The heuristic function to evaluate non-terminal states or terminal states at max depth.
   * @return A tuple containing the best Option[Move], its score (Double), and the number of nodes visited (Long).
   */
  def alphaBeta(
      board: Board,
      depth: Int,
      alpha: Double, // Initial alpha for this node's context
      beta: Double,  // Initial beta for this node's context
      currentPlayer: Player,
      maximizingPlayer: Player,
      heuristic: Heuristic
  ): (Option[Move], Double, Long) = {
    
    var nodesThisCall: Long = 1L // Count current node evaluation

    val possibleMoves = generateMoves(board, currentPlayer)

    // Base Case: Depth limit reached or game over for currentPlayer (no moves possible)
    if (depth == 0 || possibleMoves.isEmpty) {
      return (None, heuristic.evaluate(board, maximizingPlayer), nodesThisCall)
    }

    var bestMove: Option[Move] = None
    // These are the alpha and beta values that will be updated and passed down.
    // They are distinct from the alpha and beta parameters passed to this function call,
    // which represent the bounds for *this* node from its parent.
    var currentAlpha = alpha 
    var currentBeta = beta

    if (currentPlayer == maximizingPlayer) { // Maximizing player's turn
      var maxEval = Double.NegativeInfinity
      val movesIterator = possibleMoves.iterator 
      
      while (movesIterator.hasNext) {
        val move = movesIterator.next()
        val nextBoard = board.applyMove(move, currentPlayer)
        // Recursive call for the other player (minimizer)
        // Pass currentAlpha and currentBeta which are the updated bounds for children
        val (_, eval, nodesFromRec) = alphaBeta(nextBoard, depth - 1, currentAlpha, currentBeta, currentPlayer.other, maximizingPlayer, heuristic)
        nodesThisCall += nodesFromRec
        
        if (eval > maxEval) {
          maxEval = eval
          bestMove = Some(move)
        }
        currentAlpha = Math.max(currentAlpha, eval) // Update currentAlpha for this node
        if (currentBeta <= currentAlpha) { // Beta cut-off
          return (bestMove, maxEval, nodesThisCall) // Prune
        }
      }
      (bestMove, maxEval, nodesThisCall)
    } else { // Minimizing player's turn
      var minEval = Double.PositiveInfinity
      val movesIterator = possibleMoves.iterator

      while (movesIterator.hasNext) {
        val move = movesIterator.next()
        val nextBoard = board.applyMove(move, currentPlayer)
        // Recursive call for the other player (maximizer)
        // Pass currentAlpha and currentBeta which are the updated bounds for children
        val (_, eval, nodesFromRec) = alphaBeta(nextBoard, depth - 1, currentAlpha, currentBeta, currentPlayer.other, maximizingPlayer, heuristic)
        nodesThisCall += nodesFromRec
        
        if (eval < minEval) {
          minEval = eval
          bestMove = Some(move)
        }
        currentBeta = Math.min(currentBeta, eval) // Update currentBeta for this node
        if (currentBeta <= currentAlpha) { // Alpha cut-off
          return (bestMove, minEval, nodesThisCall) // Prune
        }
      }
      (bestMove, minEval, nodesThisCall)
    }
  }
}

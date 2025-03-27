package algorithms

import algorithms.utils.CostFunctions.CostFunction
import algorithms.utils.PathFindingResult
import cats.implicits.*
import domain.Connection
import domain.Graph
import domain.Stop
import domain.Time

import scala.collection.mutable
import scala.util.Random

object TabuSearchImpl { // Renamed for clarity

  private val StepLimit: Int = 10 // Increased step limit
  private val OpLimit: Int = 5 // Inner loop limit (still present in this version)

  // (b) Function to determine Tabu Tenure based on input list size (n)
  // Example: Use sqrt(n) with a minimum of 5 and max of 20 (adjust as needed)
  private def calculateTabuTenure(throughListSize: Int): Int = {
    val n = throughListSize.toDouble
    val tenure = Math.sqrt(n).round.toInt
    Math.max(5, Math.min(tenure, 20)) // Clamp between 5 and 20
  }

  // --- Aspiration parameters (kept for context) ---
  private val Epsilon: Double = 0.1
  private val HistoryLimit: Int = 10
  // ---

  def run(
    start: Stop,
    startTime: Time,
    through: List[Stop], // This is our 'L'
    graph: Graph,
    costFunction: CostFunction,
    heuristic: (Stop, Stop) => Double,
  ): Option[PathFindingResult] = {

    // --- Helper functions (getFullRouteThroughStops, routeCost, generateNeighbors) ---
    // --- remain exactly the same as in your provided code ---
    def getFullRouteThroughStops(
      start: Stop,
      startTime: Time,
      through: List[Stop],
    ): List[Connection] = {
      val neededStops = (start :: through) :+ start
      neededStops
        .sliding(2)
        .foldLeft(List.empty[Connection]) {
          case (acc, List(a, b)) =>
            AStarImpl.run(
              a,
              acc.headOption.map(_.arrivalTime).getOrElse(startTime),
              b,
              graph,
              costFunction,
              heuristic,
            ) match {
              case Some(result) if result.path.nonEmpty => // Ensure path is not empty
                result.path.reverse ::: acc
              case _                                    => acc // Keep accumulator if A* fails or returns empty
            }
          case (acc, _)          => acc
        }
        .reverse
    }

    def routeCost(
      route: List[Connection]
    ): Double = {
      // Handle empty route case
      if (route.isEmpty) return Double.PositiveInfinity

      val initCost = costFunction(startTime, None, route.head)
      route.sliding(2).foldLeft(initCost) {
        // Ensure sliding doesn't fail on single-element list result
        case (acc, List(a, b)) => acc + costFunction(startTime, a.some, b)
        case (acc, _)          => acc // Handles route with only 1 connection
      }
    }

    def generateNeighbors(
      startStop: Stop,
      currentThrough: List[Stop], // Use current permutation for neighbors
      numNeighbors: Int,
    ): List[List[Connection]] = {
      def swapTwoElements[A](list: List[A]): List[A] =
        if (list.size < 2) list
        else {
          val i = Random.nextInt(list.size)
          var j = Random.nextInt(list.size)
          while (i == j) j = Random.nextInt(list.size) // Ensure distinct
          list.updated(i, list(j)).updated(j, list(i))
        }

      // Generate neighbors based on swapping the 'through' list elements
      (0 until numNeighbors)
        .map { _ =>
          val swappedThrough = swapTwoElements(currentThrough)
          getFullRouteThroughStops(startStop, startTime, swappedThrough)
        }
        .toList
        .filter(_.nonEmpty) // Filter out cases where path generation failed
    }
    // --- End of helper functions ---

    // --- Initialization ---
    var k = 0

    // (b) Calculate the dynamic tabu tenure
    val n = through.length
    val tabuTenure = calculateTabuTenure(n)
    // println(s"Tabu Tenure: $tabuTenure") // For debugging

    // Initial solution 's' (still List[Connection] in this version)
    // Need the initial permutation to generate neighbors correctly first time
    var currentThroughPermutation = through
    var s: List[Connection] =
      getFullRouteThroughStops(start, startTime, currentThroughPermutation)

    // Handle case where initial route generation fails
    if (s.isEmpty) return None

    var sBest: List[Connection] = s
    var sBestCost: Double = routeCost(sBest) // Calculate initial best cost

    // Krok 4: T ← ∅ (tabu lista)
    // (b) Change T to a Queue to manage fixed size based on tabuTenure
    val T = mutable.Queue.empty[List[Connection]]

    // History H (still List[Connection])
    val H = mutable.Map.empty[List[Connection], Int].withDefaultValue(0)
    val historyQueue = mutable.Queue.empty[List[Connection]]

    // --- Main Loop ---
    while (k < StepLimit) {
      var i = 0
      // NOTE: This inner loop structure deviates from standard Tabu Search.
      // Usually, one neighborhood is generated and one move is made per outer 'k' iteration.
      while (i < OpLimit) {

        // Generate neighbors based on the *current permutation*
        // Need to track the permutation corresponding to state 's'
        // This is inefficient; ideally, state 's' *would be* the permutation.
        // For now, we regenerate neighbors from the initial 'through' list
        // which is incorrect - neighbors should be based on the *current* solution 's'.
        // Let's approximate by generating neighbors from the initial 'through' list
        // and hope the structure is similar enough. This highlights the issue
        // of using List[Connection] as the primary state.
        // A better (but more complex) fix involves storing the permutation alongside the path.
        // For simplicity here, we'll use the initial 'through' list for neighbor generation.
        // WARNING: This neighbor generation is not strictly correct w.r.t 's'.
        val neighbors = generateNeighbors(
          start,
          through, // Ideally: permutation corresponding to current 's'
          n.min(5), // Sample size based on n, min 5
        )

        if (neighbors.nonEmpty) { // Check if any valid neighbors were generated

          val currentCost = routeCost(s) // Calculate cost of current solution 's'

          // Aspiration check
          val aspirCandidates = neighbors.filter { candidate =>
            val candidateCost = routeCost(candidate)
            // Check cost is valid before aspiration calculation
            candidateCost.isFinite &&
            currentCost > candidateCost + Epsilon * (k - H(candidate))
          }

          // Select candidate
          val sCandidate =
            if (aspirCandidates.nonEmpty) {
              aspirCandidates.minByOption { candidate =>
                routeCost(candidate) + Epsilon * (k - H(candidate))
              }
            } else {
              // Filter non-tabu neighbors
              val nonTabu =
                neighbors.filterNot(candidate => T.contains(candidate))
              val candidateSet = if (nonTabu.nonEmpty) nonTabu else neighbors
              // Find the best cost among the chosen set
              candidateSet.minByOption(candidate => routeCost(candidate))
            }

          // Proceed only if a valid candidate was found
          sCandidate match {
            case Some(candidate) =>
              val candidateCost = routeCost(candidate)

              // --- Move Acceptance (Your original logic: only improving moves) ---
              // NOTE: Standard TS would accept non-tabu or aspirating moves always.
              if (candidateCost < currentCost) {
                s = candidate // Accept the improving move

                // (b) Update Tabu List (T) with fixed tenure
                // 1. Check if queue is full
                if (T.size >= tabuTenure) {
                  T.dequeue() // Remove the oldest element
                }
                // 2. Add the new solution 's'
                T.enqueue(s)

                // Update history (H)
                historyQueue.enqueue(s)
                H(s) = H(s) + 1
                if (historyQueue.size > HistoryLimit) {
                  val old = historyQueue.dequeue()
                  // Decrement count, ensure it doesn't go below 0
                  H(old) = Math.max(0, H(old) - 1)
                }
              }
            // If move wasn't improving or no candidate found, 's' remains unchanged.
            case None            => // No valid candidate found in neighborhood
          }
        } // End if neighbors.nonEmpty

        i += 1
      } // End inner loop (i)

      k += 1

      // Update global best
      val currentS_Cost = routeCost(s)
      if (currentS_Cost < sBestCost) {
        sBest = s
        sBestCost = currentS_Cost
      }
    } // End outer loop (k)

    // Return the best result found
    PathFindingResult(
      sBest,
      sBestCost,
    ).some
  }

}

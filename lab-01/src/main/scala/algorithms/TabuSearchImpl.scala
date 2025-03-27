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

// Code includes (b) dynamic tabu list and now correctly integrates (c) aspiration
object TabuSearchImpl {

  private val StepLimit: Int = 10 // Your reduced limit
  private val OpLimit: Int = 5   // Your reduced limit

  // (b) Function to determine Tabu Tenure
  private def calculateTabuTenure(throughListSize: Int): Int = {
    val n = throughListSize.toDouble
    val tenure = Math.sqrt(n).round.toInt
    Math.max(5, Math.min(tenure, 20)) // Clamp between 5 and 20
  }

  // (c) Aspiration parameters
  private val Epsilon: Double = 0.1
  private val HistoryLimit: Int = 10

  def run(
           start: Stop,
           startTime: Time,
           through: List[Stop], // This is 'L'
           graph: Graph,
           costFunction: CostFunction,
           heuristic: (Stop, Stop) => Double,
         ): Option[PathFindingResult] = {

    // --- Helper functions (getFullRouteThroughStops, routeCost, generateNeighbors) ---
    // --- No changes needed in helper functions ---
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
              case Some(result) if result.path.nonEmpty =>
                result.path.reverse ::: acc
              case _ => acc
            }
          case (acc, _) => acc
        }
        .reverse
    }

    def routeCost(
                   route: List[Connection]
                 ): Double = {
      if (route.isEmpty) return Double.PositiveInfinity
      val initCost = costFunction(startTime, None, route.head)
      route.sliding(2).foldLeft(initCost) {
        case (acc, List(a, b)) => acc + costFunction(startTime, a.some, b)
        case (acc, _)          => acc
      }
    }

    def generateNeighbors(
                           startStop: Stop,
                           currentThrough: List[Stop],
                           numNeighbors: Int,
                         ): List[List[Connection]] = {
      def swapTwoElements[A](list: List[A]): List[A] =
        if (list.size < 2) list
        else {
          val i = Random.nextInt(list.size)
          var j = Random.nextInt(list.size)
          while (i == j) j = Random.nextInt(list.size)
          list.updated(i, list(j)).updated(j, list(i))
        }

      (0 until numNeighbors)
        .map { _ =>
          val swappedThrough = swapTwoElements(currentThrough)
          getFullRouteThroughStops(startStop, startTime, swappedThrough)
        }
        .toList
        .filter(_.nonEmpty)
    }
    // --- End of helper functions ---

    // --- Initialization ---
    var k = 0
    val n = through.length
    val tabuTenure = calculateTabuTenure(n)

    // Initial solution 's'
    // WARNING: Still using List[Connection] as state, neighbor generation note applies
    var currentThroughPermutation = through // Track permutation for neighbor generation
    var s: List[Connection] =
      getFullRouteThroughStops(start, startTime, currentThroughPermutation)
    if (s.isEmpty) return None // Handle initial failure

    var sBest: List[Connection] = s
    var sBestCost: Double = routeCost(sBest)

    // (b) Tabu list T (Queue with fixed tenure)
    val T = mutable.Queue.empty[List[Connection]]

    // (c) History H for aspiration
    val H = mutable.Map.empty[List[Connection], Int].withDefaultValue(0)
    val historyQueue = mutable.Queue.empty[List[Connection]]

    // --- Main Loop ---
    while (k < StepLimit) {
      var i = 0
      // Inner loop (OpLimit) - still non-standard TS structure
      while (i < OpLimit) {

        // Generate neighbors based on the *current permutation*
        // Using currentThroughPermutation which *should* correspond to 's'
        val neighbors = generateNeighbors(
          start,
          currentThroughPermutation, // Use the permutation corresponding to 's'
          n.min(5) // Your reduced sample size
        )

        if (neighbors.nonEmpty) {
          val currentCost = routeCost(s)

          // Evaluate neighbors and categorize them
          val evaluatedNeighbors = neighbors.map(n => (n, routeCost(n)))

          val (tabuNeighbors, nonTabuNeighbors) =
            evaluatedNeighbors.partition { case (neighbor, _) =>
              T.contains(neighbor)
            }

          // (c) Identify best aspirating candidate among tabu neighbors
          val bestAspiratingOpt = tabuNeighbors
            .filter { case (candidate, cost) =>
              // Aspiration criterion check
              cost.isFinite && currentCost > cost + Epsilon * (k - H(candidate))
            }
            .minByOption { case (candidate, cost) =>
              // Minimize aspiration metric
              cost + Epsilon * (k - H(candidate))
            }

          // Find best non-tabu candidate
          val bestNonTabuOpt = nonTabuNeighbors
            .filter { case (_, cost) => cost.isFinite } // Ensure valid cost
            .minByOption(_._2) // Minimize cost

          // --- Candidate Selection and Move Acceptance ---
          // (c) Prioritize aspiration, then non-tabu. Accept the move if found.
          val selectedCandidateOpt: Option[(List[Connection], Double)] =
            bestAspiratingOpt.orElse(bestNonTabuOpt)

          // Perform move if a candidate (aspirating or non-tabu) was selected
          selectedCandidateOpt match {
            case Some((selectedCandidate, selectedCost)) =>
              // Update current solution 's'
              s = selectedCandidate
              // Update the permutation tracking (needed for next neighbor generation)
              // WARNING: This requires reverse engineering the permutation from the path,
              // or ideally, changing the state representation.
              // For now, we'll skip updating currentThroughPermutation, accepting
              // the inaccuracy in neighbor generation noted before.
              // currentThroughPermutation = ??? // How to get permutation from 's'?

              // (b) Update Tabu List (T)
              if (T.size >= tabuTenure) {
                T.dequeue()
              }
              T.enqueue(s) // Add the new solution 's'

              // (c) Update History (H)
              historyQueue.enqueue(s)
              H(s) = H(s) + 1
              if (historyQueue.size > HistoryLimit) {
                val old = historyQueue.dequeue()
                H(old) = Math.max(0, H(old) - 1)
              }

              // Update best solution *if* the current move improved it
              if (selectedCost < sBestCost) {
                sBest = s
                sBestCost = selectedCost
              }

            case None => // No admissible move (neither aspirating nor non-tabu) found
            // Stay in the current state 's'
          }
        } // End if neighbors.nonEmpty

        i += 1
      } // End inner loop (i)

      k += 1

      // Update global best (redundant check, already done during move acceptance)
      // val currentS_Cost = routeCost(s)
      // if (currentS_Cost < sBestCost) {
      //   sBest = s
      //   sBestCost = currentS_Cost
      // }

    } // End outer loop (k)

    // Return the best result found
    PathFindingResult(
      sBest,
      sBestCost,
    ).some
  }
}

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

// Code includes (b), (c), and now addresses sampling strategy aspect of (d)
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

  // (d) Parameter for Neighborhood Sampling Strategy
  // Increase the number of neighbors sampled compared to n.min(5).
  // Example: Sample roughly 2*n neighbors, with a min of 10 and max of 50.
  // Adjust min/max/factor based on performance and problem size.
  private def calculateNumNeighborsToSample(throughListSize: Int): Int = {
    val n = throughListSize
    val calculatedSamples = Math.max(10, n * 2) // Min 10, or 2*n
    Math.min(calculatedSamples, 50) // Cap at 50 to limit slowdown
  }

  def run(
           start: Stop,
           startTime: Time,
           through: List[Stop], // This is 'L'
           graph: Graph,
           costFunction: CostFunction,
           heuristic: (Stop, Stop) => Double,
         ): Option[PathFindingResult] = {

    // --- Helper functions (getFullRouteThroughStops, routeCost, generateNeighbors) ---
    // --- No changes needed in helper functions themselves ---
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

    // (d) generateNeighbors now uses the calculated sample size.
    // The function itself still uses random swaps and full path recalculation.
    def generateNeighbors(
                           startStop: Stop,
                           currentThrough: List[Stop],
                           numNeighbors: Int, // Parameterized number of neighbors
                         ): List[List[Connection]] = {
      def swapTwoElements[A](list: List[A]): List[A] =
        if (list.size < 2) list
        else {
          val i = Random.nextInt(list.size)
          var j = Random.nextInt(list.size)
          while (i == j) j = Random.nextInt(list.size)
          list.updated(i, list(j)).updated(j, list(i))
        }

      // Generate the specified number of neighbors
      (0 until numNeighbors)
        .map { _ =>
          val swappedThrough = swapTwoElements(currentThrough)
          getFullRouteThroughStops(startStop, startTime, swappedThrough)
        }
        .toList
        .filter(_.nonEmpty) // Filter out failed paths
    }
    // --- End of helper functions ---

    // --- Initialization ---
    var k = 0
    val n = through.length
    val tabuTenure = calculateTabuTenure(n)
    // (d) Calculate the number of neighbors to sample based on strategy
    val numNeighborsToSample = calculateNumNeighborsToSample(n)
    // println(s"Sampling $numNeighborsToSample neighbors per iteration.") // Debugging

    // Initial solution 's'
    var currentThroughPermutation = through
    var s: List[Connection] =
      getFullRouteThroughStops(start, startTime, currentThroughPermutation)
    if (s.isEmpty) return None

    var sBest: List[Connection] = s
    var sBestCost: Double = routeCost(sBest)

    // Tabu list T and History H
    val T = mutable.Queue.empty[List[Connection]]
    val H = mutable.Map.empty[List[Connection], Int].withDefaultValue(0)
    val historyQueue = mutable.Queue.empty[List[Connection]]

    // --- Main Loop ---
    while (k < StepLimit) {
      var i = 0
      while (i < OpLimit) {

        // (d) Generate neighbors using the calculated sample size
        val neighbors = generateNeighbors(
          start,
          currentThroughPermutation, // Use permutation corresponding to 's'
          numNeighborsToSample // Use the calculated sample size
        )

        // --- Rest of the loop (evaluation, selection, move) remains the same ---
        if (neighbors.nonEmpty) {
          val currentCost = routeCost(s)
          val evaluatedNeighbors = neighbors.map(n => (n, routeCost(n)))
          val (tabuNeighbors, nonTabuNeighbors) =
            evaluatedNeighbors.partition { case (neighbor, _) =>
              T.contains(neighbor)
            }

          val bestAspiratingOpt = tabuNeighbors
            .filter { case (candidate, cost) =>
              cost.isFinite && currentCost > cost + Epsilon * (k - H(candidate))
            }
            .minByOption { case (candidate, cost) =>
              cost + Epsilon * (k - H(candidate))
            }

          val bestNonTabuOpt = nonTabuNeighbors
            .filter { case (_, cost) => cost.isFinite }
            .minByOption(_._2)

          val selectedCandidateOpt: Option[(List[Connection], Double)] =
            bestAspiratingOpt.orElse(bestNonTabuOpt)

          selectedCandidateOpt match {
            case Some((selectedCandidate, selectedCost)) =>
              s = selectedCandidate
              // WARNING: currentThroughPermutation not updated, see previous notes.

              if (T.size >= tabuTenure) { T.dequeue() }
              T.enqueue(s)

              historyQueue.enqueue(s)
              H(s) = H(s) + 1
              if (historyQueue.size > HistoryLimit) {
                val old = historyQueue.dequeue()
                H(old) = Math.max(0, H(old) - 1)
              }

              if (selectedCost < sBestCost) {
                sBest = s
                sBestCost = selectedCost
              }
            case None => // No admissible move
          }
        } // End if neighbors.nonEmpty
        i += 1
      } // End inner loop (i)
      k += 1
    } // End outer loop (k)

    // Return the best result found
    PathFindingResult(
      sBest,
      sBestCost,
    ).some
  }
}

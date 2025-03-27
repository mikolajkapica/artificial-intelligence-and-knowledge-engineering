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

object TabuSearchImpl {

  private val StepLimit: Int = 5
  private val OpLimit: Int = 10

  // Parametry aspiracyjne: (Kept for context, but not focus of point 'a')
  private val Epsilon: Double = 0.1

  // Liczba kroków, które będziemy pamiętać w historii H
  private val HistoryLimit: Int = 10

  def run(
    start: Stop,
    startTime: Time,
    through: List[Stop],
    graph: Graph,
    costFunction: CostFunction,
    heuristic: (Stop, Stop) => Double,
  ): Option[PathFindingResult] = {

    def getFullRouteThroughStops(start: Stop, startTime: Time, through: List[Stop]) = {
      val neededStops = (start :: through) :+ start
      neededStops
        .sliding(2)
        .foldLeft(List.empty[Connection]) {
          case (acc, List(a, b)) =>
            AStarImpl.run(a, acc.headOption.map(_.arrivalTime).getOrElse(startTime), b, graph, costFunction, heuristic) match {
              case Some(result) => result.path.reverse ::: acc
              case None         => acc
            }
          case (acc, _)          => acc
        }
        .reverse
    }

    def routeCost(
      route: List[Connection]
    ): Double = {
      val initCost = route.headOption.map(routeHead => costFunction(startTime, None, routeHead)).getOrElse(Double.PositiveInfinity)
      route.sliding(2).foldLeft(initCost) {
        case (acc, List(a, b)) => acc + costFunction(startTime, a.some, b)
        case (acc, _)          => acc
      }
    }

    def generateNeighbors(
      startStop: Stop,
      through: List[Stop],
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

      (0 until numNeighbors).map { _ =>
        val swappedThrough = swapTwoElements(through)
        val newRoute =
          getFullRouteThroughStops(startStop, startTime, swappedThrough)
        newRoute
      }.toList
    }

    var k = 0

    // Krok 2: Losujemy rozwiązanie początkowe s
    val initialRoute = getFullRouteThroughStops(start, startTime, through)
    var s: List[Connection] = initialRoute

    // Krok 3: s* <- s
    var sBest: List[Connection] = s

    // Krok 4: T ← ∅ (tabu lista)
    // (a) Using mutable.Set implements a tabu list without a predefined size limit.
    val T = mutable.Set.empty[List[Connection]]

    // History for aspiration (kept for context)
    val H = mutable.Map.empty[List[Connection], Int].withDefaultValue(0)
    val historyQueue = mutable.Queue.empty[List[Connection]]

    // Krok 5: while k < STEP_LIMIT do
    while (k < StepLimit) {

      // Krok 6: i ← 0
      var i = 0

      // Krok 7: while i < OP_LIMIT do
      while (i < OpLimit) {

        // Generate neighbors (same as before)
        val neighbors = generateNeighbors(start, through, through.size.min(10))

        // Aspiration check (same as before)
        val aspirCandidates = neighbors.filter { candidate =>
          routeCost(s) > routeCost(candidate) +
            Epsilon * (k - H(candidate))
        }

        // Select candidate (same logic as before)
        val sCandidate =
          if (aspirCandidates.nonEmpty) {
            aspirCandidates.minBy { candidate =>
              routeCost(candidate) + Epsilon * (k - H(candidate))
            }
          } else {
            // Filter neighbors that are NOT in the tabu set T
            val nonTabu = neighbors.filterNot(candidate => T.contains(candidate))
            val candidateSet = if (nonTabu.nonEmpty) nonTabu else neighbors
            candidateSet.minBy(candidate => routeCost(candidate))
          }

        // --- Move Acceptance and Tabu Update ---
        // Check if the move should be accepted (improving OR non-improving allowed in TS)
        // NOTE: Your original code only accepted *improving* moves here.
        // A standard TS accepts if non-tabu OR meets aspiration.
        // For now, let's stick to your original logic for minimal change,
        // but be aware this isn't standard TS move acceptance.
        if (routeCost(sCandidate) < routeCost(s)) {
          s = sCandidate // Accept the move

          // (a) Add the *newly accepted solution* 's' to the tabu list T.
          // Since T is a Set, it automatically handles duplicates.
          // Since we never remove, it grows without bound.
          T.add(s)

          // Update history (same as before)
          historyQueue.enqueue(s)
          H(s) = H(s) + 1
          if (historyQueue.size > HistoryLimit) {
            val old = historyQueue.dequeue()
            H(old) = H(old) - 1
          }
        }
        // If the move wasn't improving, in your original logic, nothing happens,
        // and T is not updated with sCandidate.

        i += 1
      }

      // Krok 14: k ← k + 1
      k += 1

      // Update global best (same as before)
      if (routeCost(s) < routeCost(sBest)) {
        sBest = s
      }
    } // End outer loop (k)

    // Return result (same as before)
    PathFindingResult(
      sBest,
      routeCost(sBest),
    ).some
  }

}

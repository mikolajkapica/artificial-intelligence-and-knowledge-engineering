package algorithms

import algorithms.utils.CostFunctions.CostFunction
import algorithms.utils.PathFindingResult
import cats.implicits.*
import domain.{Connection, Graph, Stop, Time}

import scala.collection.mutable
import scala.util.Random

object TabuSearchImpl {

  private val StepLimit: Int = 10
  private val OpLimit: Int = 5

  private def calculateTabuTenure(throughListSize: Int): Int = {
    val n = throughListSize.toDouble
    val tenure = Math.sqrt(n).round.toInt
    Math.max(5, Math.min(tenure, 20))
  }

  private val Epsilon: Double = 0.1
  private val HistoryLimit: Int = 10

  private def calculateNumNeighborsToSample(throughListSize: Int): Int = {
    val n = throughListSize
    val calculatedSamples = Math.max(10, n * 2)
    Math.min(calculatedSamples, 50)
  }

  def run(
    start: Stop,
    startTime: Time,
    through: List[Stop],
    graph: Graph,
    costFunction: CostFunction,
    heuristic: (Stop, Stop) => Double,
  ): Option[PathFindingResult] = {

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
              case _                                    => acc
            }
          case (acc, _)          => acc
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

    var k = 0
    val n = through.length
    val tabuTenure = calculateTabuTenure(n)
    val numNeighborsToSample = calculateNumNeighborsToSample(n)

    var currentThroughPermutation = through
    var s: List[Connection] =
      getFullRouteThroughStops(start, startTime, currentThroughPermutation)
    if (s.isEmpty) return None

    var sBest: List[Connection] = s
    var sBestCost: Double = routeCost(sBest)

    val T = mutable.Queue.empty[List[Connection]]
    val H = mutable.Map.empty[List[Connection], Int].withDefaultValue(0)
    val historyQueue = mutable.Queue.empty[List[Connection]]

    while (k < StepLimit) {
      var i = 0
      while (i < OpLimit) {

        val neighbors = generateNeighbors(
          start,
          currentThroughPermutation,
          numNeighborsToSample,
        )

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
            case None                                    =>
          }
        }
        i += 1
      }
      k += 1
    }

    PathFindingResult(
      sBest,
      sBestCost,
    ).some
  }

}

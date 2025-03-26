package algorithms

import cats.implicits.*
import domain.Connection
import domain.Graph
import domain.Stop

import scala.collection.mutable
import scala.util.Random

object TabuSearchKnox {

  private val StepLimit: Int = 100
  private val OpLimit: Int = 50

  private def routeCost(
    route: List[Stop],
    cost: (Stop, Stop) => Double,
  ): Double =
    route.sliding(2).foldLeft(0.0) {
      case (acc, List(a, b)) => acc + cost(a, b)
      case (acc, _)          => acc
    }

  private def generateNeighbors(route: List[Stop]): List[List[Stop]] =
    if (route.size <= 3) List(route)
    else {
      val indices = 1 until (route.size - 1)
      (for {
        i <- indices
        j <- indices if i < j
      } yield {
        val newRoute = route.toArray
        val tmp = newRoute(i)
        newRoute(i) = newRoute(j)
        newRoute(j) = tmp
        newRoute.toList
      }).toList
    }

  private def buildConnectionsList(
    route: List[Stop],
    graph: Graph,
  ): Option[List[Connection]] =
    route.sliding(2).toList.traverse {
      case List(a, b) =>
        graph.get(a).flatMap { connections =>
          connections.find(conn => conn.endStop == b)
        }
      case _          => None
    }

  def run(
    start: Stop,
    through: List[Stop],
    graph: Graph,
    cost: (Stop, Stop) => Double,
  ): Option[PathFindingResult] = {
    // Krok 1: k ← 0
    var k = 0

    // Krok 2: Losuj rozwiązanie początkowe s
    val initialRoute = start :: Random.shuffle(through) ::: List(start)
    var s: List[Stop] = initialRoute

    // Krok 3: s* <- s (globalne optimum)
    var sBest: List[Stop] = s

    // Krok 4: T <- ∅ (tabu lista)
    val T = mutable.Set.empty[List[Stop]]

    // Krok 5: while k < STEP_LIMIT do
    while (k < StepLimit) {

      // Krok 6: i ← 0
      var i = 0

      // Krok 7: while i < OP_LIMIT do
      while (i < OpLimit) {

        // Krok 8: określ N(s) – generujemy sąsiedztwo bieżącego rozwiązania
        val neighbors = generateNeighbors(s)

        // Wyznacz aspiracyjne rozwiązania A: te, które są tabu, ale mają lepszy
        // koszt niż globalne optimum.
        val candidateSet = {
          val nonTabuCandidates = neighbors.filter(n => !T.contains(n))
          val aspiratedCandidates = neighbors.filter(n => T.contains(n) && routeCost(n, cost) < routeCost(sBest, cost))
          val combined = nonTabuCandidates ++ aspiratedCandidates
          if (combined.nonEmpty) combined else neighbors
        }

        // Krok 9: wybierz najlepszy s' ∈ (N(s) \ T) ∪ A
        val sPrime = candidateSet.minBy(n => routeCost(n, cost))

        // Krok 10: zaktualizuj T – dodaj bieżące rozwiązanie do tabu listy
        T.add(s)

        // Krok 11-12: if D(s') < D(s) then aktualizuj lokalne optimum s ← s'
        if (routeCost(sPrime, cost) < routeCost(s, cost)) {
          s = sPrime
        }

        // Krok 13: i ← i + 1
        i += 1
      }
      // Krok 14: k ← k + 1
      k += 1

      // Krok 15: if D(s) < D(s*) then s* ← s
      if (routeCost(s, cost) < routeCost(sBest, cost)) {
        sBest = s
      }
    }

    // Po zakończeniu algorytmu – przekształcamy trasę (listę przystanków)
    // na listę połączeń
    buildConnectionsList(sBest, graph).map { connections =>
      PathFindingResult(connections, routeCost(sBest, cost))
    }
  }

}

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

  Random.setSeed(42)

  private val StepLimit: Int = 5
  private val OpLimit: Int = 10

  // Parametry aspiracyjne:
  // ε musi być z przedziału (0, 1)
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
      // swap
      def swapTwoElements[A](list: List[A]): List[A] = {
        val i = Random.nextInt(list.size)
        val j = Random.nextInt(list.size)
        list.updated(i, list(j)).updated(j, list(i))
      }

      (0 to numNeighbors).map { _ =>
        val swappedThrough = swapTwoElements(through)
        val newRoute = getFullRouteThroughStops(startStop, startTime, swappedThrough)
        newRoute
      }.toList
    }

    // Krok 1: k ← 0
    var k = 0

    // Krok 2: Losujemy rozwiązanie początkowe s
    val initialRoute = getFullRouteThroughStops(start, startTime, through)

    var s: List[Connection] = initialRoute

    // Krok 3: s* <- s
    var sBest: List[Connection] = s

    // Krok 4: T ← ∅ (tabu lista)
    val T = mutable.Set.empty[List[Connection]]

    // Dodatkowo: historia modyfikacji (H) oraz kolejka, która utrzymuje historię
    // ostatnich HistoryLimit kroków.
    val H = mutable.Map.empty[List[Connection], Int].withDefaultValue(0)
    val historyQueue = mutable.Queue.empty[List[Connection]]

    // Krok 5: while k < STEP_LIMIT do
    while (k < StepLimit) {

      // Krok 6: i ← 0
      var i = 0

      // Krok 7: while i < OP_LIMIT do
      while (i < OpLimit) {

        // Generujemy sąsiedztwo bieżącego rozwiązania
        val neighbors = generateNeighbors(start, through, 10)

        // W pierwszej kolejności sprawdzamy aspiracyjne kryterium:
        // dla kandydata s_i obliczamy A_i = f(s_i) + ε*(k − H(s_i)).
        // Jeśli f(s) > A_i, rozwiązanie s_i jest atrakcyjne, niezależnie od tego,
        // czy należy do listy tabu.
        val aspirCandidates = neighbors.filter { candidate =>
          routeCost(s) > routeCost(candidate) +
            Epsilon * (k - H(candidate))
        }

        // Wybieramy kandydata:
        val sCandidate =
          if (aspirCandidates.nonEmpty) {
            // Wśród aspiracyjnych kandydujemy wybieramy ten, który minimalizuje
            // wartość f(s_i) + ε*(k − H(s_i))
            aspirCandidates.minBy { candidate =>
              routeCost(candidate) + Epsilon * (k - H(candidate))
            }
          } else {
            // Brak kandydatów spełniających aspiracyjne kryterium:
            // wybieramy najlepszy spośród niedozwolonych (non-taboo) lub wszelkich sąsiadów.
            val nonTabu = neighbors.filterNot(T.contains)
            val candidateSet = if (nonTabu.nonEmpty) nonTabu else neighbors
            candidateSet.minBy(candidate => routeCost(candidate))
          }

        // Dodajemy bieżące rozwiązanie do tablicy tabu
        T.add(s)

        // Jeżeli kandydat poprawia lokalny koszt, akceptujemy posunięcie.
        if (routeCost(sCandidate) < routeCost(s)) {
          s = sCandidate

          // Aktualizujemy historię – zwiększamy licznik modyfikacji dla s
          historyQueue.enqueue(s)
          H(s) = H(s) + 1

          // Utrzymujemy okno historyczne o długości HistoryLimit:
          if (historyQueue.size > HistoryLimit) {
            val old = historyQueue.dequeue()
            H(old) = H(old) - 1
          }
        }

        i += 1
      }

      // Krok 14: k ← k + 1
      k += 1

      // Aktualizacja globalnego optimum
      if (routeCost(s) < routeCost(sBest)) {
        sBest = s
      }
    }

    // Przekształcamy trasę sBest (listę przystanków) na listę połączeń.
    PathFindingResult(
      sBest,
      routeCost(sBest),
    ).some
  }

}

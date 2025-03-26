package algorithms

import cats.implicits.*
import domain.{Connection, Graph, Stop, Time}

import scala.collection.mutable
import scala.util.Random

object TabuSearchKnox {

  private val StepLimit: Int = 100
  private val OpLimit: Int = 50

  // Parametry aspiracyjne:
  // ε musi być z przedziału (0, 1)
  private val Epsilon: Double = 0.1
  // Liczba kroków, które będziemy pamiętać w historii H
  private val HistoryLimit: Int = 10

   def routeCost(
    route: List[Stop],
    cost: (Stop, Stop) => Double,
  ): Double =
    route.sliding(2).foldLeft(0.0) {
      case (acc, List(a, b)) => acc + cost(a, b)
      case (acc, _)          => acc
    }

  def generateNeighbors(route: List[Stop]): List[List[Stop]] =
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

  def buildConnectionsList(
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
    startTime: Time,
    through: List[Stop],
    graph: Graph,
    cost: CostFunction,
  ): Option[PathFindingResult] = {
    // Krok 1: k ← 0
    var k = 0

    // Krok 2: Losujemy rozwiązanie początkowe s
    val initialRoute = start :: Random.shuffle(through) ::: List(start)
    var s: List[Stop] = initialRoute

    // Krok 3: s* <- s
    var sBest: List[Stop] = s

    // Krok 4: T ← ∅ (tabu lista)
    val T = mutable.Set.empty[List[Stop]]

    // Dodatkowo: historia modyfikacji (H) oraz kolejka, która utrzymuje historię
    // ostatnich HistoryLimit kroków.
    val H = mutable.Map.empty[List[Stop], Int].withDefaultValue(0)
    val historyQueue = mutable.Queue.empty[List[Stop]]

    // Krok 5: while k < STEP_LIMIT do
    while (k < StepLimit) {

      // Krok 6: i ← 0
      var i = 0

      // Krok 7: while i < OP_LIMIT do
      while (i < OpLimit) {

        // Generujemy sąsiedztwo bieżącego rozwiązania
        val neighbors = generateNeighbors(s)

        // W pierwszej kolejności sprawdzamy aspiracyjne kryterium:
        // dla kandydata s_i obliczamy A_i = f(s_i) + ε*(k − H(s_i)).
        // Jeśli f(s) > A_i, rozwiązanie s_i jest atrakcyjne, niezależnie od tego,
        // czy należy do listy tabu.
        val aspirCandidates = neighbors.filter { candidate =>
          routeCost(s, cost) > routeCost(candidate, cost) +
            Epsilon * (k - H(candidate))
        }

        // Wybieramy kandydata:
        val sCandidate =
          if (aspirCandidates.nonEmpty) {
            // Wśród aspiracyjnych kandydujemy wybieramy ten, który minimalizuje
            // wartość f(s_i) + ε*(k − H(s_i))
            aspirCandidates.minBy { candidate =>
              routeCost(candidate, cost) + Epsilon * (k - H(candidate))
            }
          } else {
            // Brak kandydatów spełniających aspiracyjne kryterium:
            // wybieramy najlepszy spośród niedozwolonych (non-taboo) lub wszelkich sąsiadów.
            val nonTabu = neighbors.filterNot(T.contains)
            val candidateSet = if (nonTabu.nonEmpty) nonTabu else neighbors
            candidateSet.minBy(candidate => routeCost(candidate, cost))
          }

        // Dodajemy bieżące rozwiązanie do tablicy tabu
        T.add(s)

        // Jeżeli kandydat poprawia lokalny koszt, akceptujemy posunięcie.
        if (routeCost(sCandidate, cost) < routeCost(s, cost)) {
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
      if (routeCost(s, cost) < routeCost(sBest, cost)) {
        sBest = s
      }
    }

    // Przekształcamy trasę sBest (listę przystanków) na listę połączeń.
    buildConnectionsList(sBest, graph).map { connections =>
      PathFindingResult(connections, routeCost(sBest, cost))
    }
  }

}

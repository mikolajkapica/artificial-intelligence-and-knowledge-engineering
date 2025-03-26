package algorithms

import cats.implicits.*
import domain.Connection
import domain.Graph
import domain.Stop
import domain.Time

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.util.Try

object TabuSearchKnox {

  Random.setSeed(42)

  private val StepLimit: Int = 100
  private val OpLimit: Int = 50

  // Parametry aspiracyjne:
  // ε musi być z przedziału (0, 1)
  private val Epsilon: Double = 0.1

  // Liczba kroków, które będziemy pamiętać w historii H
  private val HistoryLimit: Int = 10
  
  def getFullRouteThroughPoints()

  def initRoute(
    startTime: Time,
    start: Stop,
    through: List[Stop],
    costFunction: CostFunction,
    graph: Graph,
  ): List[Connection] = {

    // 




    val path = mutable.ListBuffer.empty[Connection]
    val used = mutable.Set.empty[Stop]

    used += start

    def randomNextStopWithLowestCost(current: Stop) = {
      val possibleStops = graph(current).groupBy(_.endStop).values.toList
      val nextStep = possibleStops(Random.nextInt(possibleStops.size))
      val nextConnection = nextStep.minBy(connection => costFunction(startTime, path.headOption, connection))
      nextConnection
    }

    var current = start

    while (!used.contains(current) || !through.forall(used.contains)) {
      val nextConnection = randomNextStopWithLowestCost(current)
      path.prepend(nextConnection)
      used += nextConnection.endStop
      current = nextConnection.endStop
    }

    path.reverse.toList
  }

  def run(
    start: Stop,
    startTime: Time,
    through: List[Stop],
    graph: Graph,
    costFunction: CostFunction,
  ): Option[PathFindingResult] = {

    def getBestConnection(graph: Graph, from: Connection, to: Stop): Connection = {
      graph
        .get(from.endStop)
        .toList
        .flatMap(x => x.groupBy(_.endStop))
        .find(x => x._1 == to)
        .toList
        .flatMap(_._2)
        .minByOption(connection => costFunction(startTime, from.some, connection))
        .get
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

    def stopsToConnections(stops: List[Stop]): List[Connection] = {
      val first = stops.head
      val second = stops.tail.head
      val firstConnection = graph(first).filter(_.endStop == second).minBy(stop => costFunction(startTime, None, stop))

      stops
        .sliding(2)
        .drop(1)
        .foldLeft(List(firstConnection)) {
          case (acc, List(_, b)) =>
            val nextConnection = getBestConnection(graph, acc.head, b)
            nextConnection :: acc
          case (acc, _)          => acc
        }
        .reverse
    }

    def generateNeighbors(
      graph: Map[Stop, Set[Connection]],
      route: List[Connection],
      startStop: Stop,
      requiredStops: Set[Stop],
      numNeighbors: Int,
      maxAttempts: Int = 1000, // Safety limit
    ): List[List[Connection]] = {

      //    // --- Input Validation (same as previous version) ---
      //    if (route.isEmpty) return Left("Input route cannot be empty.")
      //    if (route.head.startStop != startStop)
      //      return Left(s"Route must start with ${startStop.name}.")
      //    if (route.last.endStop != startStop)
      //      return Left(s"Route must end with ${startStop.name}.")
      //
      val currentStops: List[Stop] =
        route.headOption.map(_.startStop).toList ++ route.map(_.endStop)
      val currentStopsSet = currentStops.toSet
      val missingRequired = requiredStops.diff(currentStopsSet)
      //    if (missingRequired.nonEmpty)
      //      return Left(
      //        s"Route missing required stops: ${missingRequired.map(_.name).mkString(", ")}"
      //      )
      if (currentStops.length < 3) return List.empty // Need cycle > 2 stops
      //    // --- End Validation ---

      val n = currentStops.length // Includes start/end stop twice
      val neighbors = mutable.Set[List[Stop]]() // Use Set for auto-uniqueness
      var attempts = 0

      // Helper to check connection
      def hasConnection(from: Stop, to: Stop): Boolean =
        graph.get(from).exists(_.exists(_.endStop == to))

      val allStopsInGraph = graph.keys.toList // For Add move

      // --- Random Generation Loop ---
      while (neighbors.size < numNeighbors && attempts < maxAttempts) {
        attempts += 1
        val moveType = Random.nextInt(4) // 0: Swap, 1: Add, 2: Remove, 3: Relocate

        Try { // Use Try to catch potential errors like invalid indices
          moveType match {
            // --- Move 0: Swap Adjacent (excluding start/end) ---
            case 0 if n > 3 => // Need at least 4 stops for internal swap (S->X->Y->S)
              val i = Random.nextInt(n - 3) + 1 // Index from 1 to n-3
              val s_i = currentStops(i)
              val s_i_plus_1 = currentStops(i + 1)
              val s_i_minus_1 = currentStops(i - 1)
              val s_i_plus_2 = currentStops(i + 2)

              if (
                hasConnection(s_i_minus_1, s_i_plus_1) &&
                hasConnection(s_i_plus_1, s_i) &&
                hasConnection(s_i, s_i_plus_2)
              ) {
                val neighbor = currentStops.patch(i, Seq(s_i_plus_1, s_i), 2)
                neighbors.add(neighbor)
              }

            // --- Move 1: Add a Stop ---
            case 1 if allStopsInGraph.nonEmpty =>
              val i = Random.nextInt(n - 1) // Index from 0 to n-2 (insertion point *before*)
              val s_i = currentStops(i)
              val s_i_plus_1 = currentStops(i + 1)
              val s_new =
                allStopsInGraph(
                  Random.nextInt(allStopsInGraph.size)
                ) // Pick random stop

              if (hasConnection(s_i, s_new) && hasConnection(s_new, s_i_plus_1)) {
                val neighbor = currentStops.patch(i + 1, Seq(s_new), 0)
                neighbors.add(neighbor)
              }

            // --- Move 2: Remove a Stop (non-required, non-start/end) ---
            case 2 if n > 3 => // Need at least 4 stops to have a removable one
              val i = Random.nextInt(n - 2) + 1 // Index from 1 to n-2
              val s_i = currentStops(i)

              if (s_i != startStop && !requiredStops.contains(s_i)) {
                val s_i_minus_1 = currentStops(i - 1)
                val s_i_plus_1 = currentStops(i + 1)
                if (hasConnection(s_i_minus_1, s_i_plus_1)) {
                  val neighbor = currentStops.patch(i, Nil, 1)
                  neighbors.add(neighbor)
                }
              }

            // --- Move 3: Relocate a Stop (non-required, non-start/end) ---
            case 3 if n > 3 => // Need at least 4 stops
              // 1. Select stop to remove
              val removeIndex = Random.nextInt(n - 2) + 1 // Index 1 to n-2
              val stopToRelocate = currentStops(removeIndex)

              if (
                stopToRelocate != startStop && !requiredStops.contains(
                  stopToRelocate
                )
              ) {
                val s_remove_prev = currentStops(removeIndex - 1)
                val s_remove_next = currentStops(removeIndex + 1)

                // Check feasibility of removing
                if (hasConnection(s_remove_prev, s_remove_next)) {
                  // 2. Select insertion point (index *before* insertion)
                  // Cannot insert right before the original next stop
                  var insertBeforeIndex = Random.nextInt(n - 1) // Index 0 to n-2
                  while (insertBeforeIndex == removeIndex)
                    // Avoid inserting back in the same gap
                    insertBeforeIndex = Random.nextInt(n - 1)

                  val s_insert_prev = currentStops(insertBeforeIndex)
                  val s_insert_next = currentStops(insertBeforeIndex + 1)

                  // Check feasibility of inserting
                  if (
                    hasConnection(s_insert_prev, stopToRelocate) &&
                    hasConnection(stopToRelocate, s_insert_next)
                  ) {
                    // Perform removal and insertion
                    val tempRoute = currentStops.patch(removeIndex, Nil, 1)
                    // Adjust insertion index if it was after the removal index
                    val actualInsertIndex =
                      if (insertBeforeIndex < removeIndex) insertBeforeIndex
                      else insertBeforeIndex - 1

                    val neighbor =
                      tempRoute.patch(actualInsertIndex + 1, Seq(stopToRelocate), 0)
                    neighbors.add(neighbor)
                  }
                }
              }
            case _          => // No valid move possible for this type / route length
          }
        } // Ignore Try failures (e.g., index out of bounds if logic flawed)
      } // End while loop

      neighbors.toList.map(stopsToConnections)
    }

    // Krok 1: k ← 0
    var k = 0

    // Krok 2: Losujemy rozwiązanie początkowe s
    val initialRoute = initRoute(
      startTime,
      start,
      through,
      costFunction,
      graph,
    )
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
        val neighbors = generateNeighbors(graph, s, start, through.toSet, 10)

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

package algorithms

import algorithms.utils.CostFunctions.CostFunction
import algorithms.utils.{PathFindingResult, reconstructPath}
import domain.Connection
import domain.Graph
import domain.Stop
import domain.Time

import scala.collection.mutable

object AStarOptimizedImpl {

  def run(
    start: Stop,
    startTime: Time,
    end: Stop,
    graph: Graph,
    costFunction: CostFunction,
    heuristic: (Stop, Stop) => Double
  ): Option[PathFindingResult] = {

    val gScore = mutable.Map(start -> 0.0).withDefaultValue(Double.PositiveInfinity)
    val hScore = mutable.Map(start -> heuristic(start, end))
    val fScore = mutable.Map(start -> (gScore(start) + hScore(start))).withDefaultValue(Double.PositiveInfinity)

    // OPTYMALIZACJA 1: Użycie Kolejki Priorytetowej (Min-Heap)
    // Zamiast przeszukiwać całą listę 'opened' w każdym kroku (O(N)),
    // używamy kolejki priorytetowej. Pozwala ona na pobranie węzła
    // z najmniejszym fScore w czasie O(log N), co znacząco przyspiesza algorytm.
    // Używamy .reverse, ponieważ domyślnie PriorityQueue w Scali to Max-Heap.
    val opened = mutable.PriorityQueue(start)(Ordering.by(fScore(_)).reverse)

    val closed = mutable.Set.empty[Stop]
    val predecessors = mutable.Map.empty[Stop, Connection]

    while (opened.nonEmpty) {
      // OPTYMALIZACJA 1 (Ciąg dalszy): Pobieranie węzła z kolejki
      // Pobranie węzła o najmniejszym fScore zajmuje teraz O(log N).
      val currentNode = opened.dequeue()

      // OPTYMALIZACJA 2: Obsługa "przestarzałych" wpisów w kolejce
      // Jeśli aktualizujemy koszt węzła, który jest już w kolejce,
      // dodajemy go ponownie z nowym (niższym) kosztem. Dlatego musimy
      // upewnić się, że przetwarzamy dany węzeł tylko raz, ignorując
      // ewentualne starsze wpisy z wyższym kosztem, które mogły pozostać w kolejce.
      if (!closed.contains(currentNode)) {

        // Przeniesienie dodania do 'closed' tutaj gwarantuje,
        // że nawet jeśli węzeł pojawi się w kolejce wielokrotnie
        // (z powodu aktualizacji ścieżki), przetworzymy go tylko raz.
        closed.add(currentNode)

        // Sprawdzenie warunku końcowego PRZED eksploracją sąsiadów
        // jest bardziej efektywne, jeśli celem jest sam start.
        if (currentNode == end) {
          return Some(
            PathFindingResult(
              path = reconstructPath(predecessors.toMap, end),
              cost = gScore(end),
            )
          )
        }

        // Eksploracja sąsiadów
        for (connection <- graph.getOrElse(currentNode, List.empty)) {
          val neighbor = connection.endStop

          // Obliczamy potencjalny nowy koszt dotarcia do sąsiada
          val tentativeGScore = gScore(currentNode) + costFunction(
            startTime, // Uwaga: startTime jest stałe, może być potrzebna aktualna godzina w currentNode?
            predecessors.get(currentNode),
            connection,
          )

          // Jeśli znaleźliśmy lepszą ścieżkę do sąsiada
          if (tentativeGScore < gScore(neighbor)) {
            // Aktualizujemy informacje o ścieżce
            predecessors.update(neighbor, connection)
            gScore.update(neighbor, tentativeGScore)
            hScore.update(neighbor, heuristic(neighbor, end)) // hScore liczymy tylko raz, jeśli jest spójna
            fScore.update(neighbor, gScore(neighbor) + hScore(neighbor))

            // OPTYMALIZACJA 1 (Ciąg dalszy): Dodawanie/Aktualizacja w kolejce
            // Dodajemy sąsiada do kolejki priorytetowej. Jeśli sąsiad już tam był
            // z wyższym kosztem fScore, ten nowy wpis będzie miał wyższy priorytet.
            // Stary wpis zostanie zignorowany później dzięki sprawdzeniu 'closed'.
            // Operacja enqueue ma złożoność O(log N).
            opened.enqueue(neighbor)

            // Usunięcie z 'closed' nie jest już potrzebne w tym miejscu,
            // ponieważ sprawdzamy 'closed' na początku pętli (Optymalizacja 2).
            // Jeśli węzeł był w 'closed' i znaleźliśmy do niego lepszą ścieżkę,
            // zostanie on ponownie dodany do 'opened' i przetworzony w przyszłości.
          }
        }
      }
    }

    None
  }

}

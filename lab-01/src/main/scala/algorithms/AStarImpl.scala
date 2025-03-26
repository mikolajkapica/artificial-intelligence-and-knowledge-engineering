package algorithms

import algorithms.DijkstraImpl.cost

import scala.collection.mutable
import cats.Eq
import cats.syntax.eq.*
import cats.syntax.option.*
import domain.Connection
import domain.Graph
import domain.Stop
import domain.Time

import scala.annotation.tailrec

object AStarImpl {

  private[algorithms] def reconstructPath(
    predecessors: Map[Stop, Connection],
    end: Stop,
  ): List[Connection] = {
    val path = mutable.ListBuffer.empty[Connection]
    var current = end

    while (predecessors.contains(current)) {
      val connection = predecessors(current)
      path.prepend(connection)
      current = connection.startStop
    }

    path.toList
  }

  def run(
    start: Stop,
    startTime: Time,
    end: Stop,
    graph: Graph,
  ): Option[PathFindingResult] = {
    val heuristic = lengthHeuristic

    // 1: poczatek.g ← 0
    val gScore = mutable.Map(start -> 0.0)
    // 2: poczatek.h ← 0
    val hScore = mutable.Map(start -> heuristic(start, end))
    // 3: poczatek.f ← poczatek.g + poczatek.h
    val fScore = mutable.Map(start -> (gScore(start) + hScore(start)))
    // 4: otwarte ← list([poczatek])
    val opened = mutable.Set(start)
    // 5: zamkniete ← list()
    val closed = mutable.Set.empty[Stop]

    val predecessors = mutable.Map.empty[Stop, Connection]

    // 6: while len(otwarte) > 0 do
    while (opened.nonEmpty) {
      // 7: wezel ← null
      var currentNode: Stop = null
      // 8: koszt_wezla ← +Inf
      var currentCost = Double.PositiveInfinity

      // 9: for wezel_testowy in otwarte do
      for (node <- opened)
        // 10: if f(wezel_testowy) < koszt_wezla then
        if (fScore.getOrElse(node, Double.PositiveInfinity) < currentCost) {
          // 11: wezel ← wezel_testowy
          currentNode = node
          // 12: koszt_wezla ← f(wezel_testowy)
          currentCost = fScore(node)
        }

      // 13: if wezel == koniec then
      if (currentNode == end) {
        // 14: Rozwiązanie znalezione
        return Some(
          PathFindingResult(
            path = reconstructPath(predecessors.toMap, end),
            cost = gScore(end),
          )
        )
      }

      // 15: otwarte ← (otwarte − wezel)
      opened.remove(currentNode)
      // 16: zamkniete ← (zamkniete + wezel)
      closed.add(currentNode)

      // 17: for wezel_nastepny in sasiedztwo(wezel) do
      for (connection <- graph(currentNode)) {
        val neighbor = connection.endStop
        val tentativeGScore = gScore(currentNode) + cost(startTime, predecessors.get(currentNode), connection)

        // 18: if wezel_nastepny not in otwarte and wezel_nastepny not in zamkniete then
        if (!opened.contains(neighbor) && !closed.contains(neighbor)) {
          // 19: otwarte ← (otwarte + wezel_nastepny)
          opened.add(neighbor)
          // 20: wezel_nastepny.h = h(wezel_nastepny, koniec)
          hScore.update(neighbor, heuristic(neighbor, end))
          // 21: wezel_nastepny.g = wezel.g + g(wezel, wezel_nastepny)
          gScore.update(neighbor, tentativeGScore)
          // 22: wezel_nastepny.f = wezel_nastepny.g + wezel_nastepny.h
          fScore.update(neighbor, gScore(neighbor) + hScore(neighbor))

          predecessors.update(neighbor, connection)

        } else {
          // 24: if wezel_nastepny.g > wezel.g + g(wezel, wezel_nastepny) then
          if (tentativeGScore < gScore.getOrElse(neighbor, Double.PositiveInfinity)) {
            // 25: wezel_nastepny.g = wezel.g + g(wezel, wezel_nastepny)
            gScore.update(neighbor, tentativeGScore)
            // 26: wezel_nastepny.f = wezel_nastepny.g + wezel_nastepny.h
            fScore.update(neighbor, gScore(neighbor) + hScore(neighbor))

            predecessors.update(neighbor, connection)

            // 27: if wezel_nastepny in zamkniete then
            if (closed.contains(neighbor)) {
              // 28: otwarte ← (otwarte + wezel_nastepny)
              opened.add(neighbor)
              // 29: zamkniete ← (zamkniete − wezel_nastepny)
              closed.remove(neighbor)
            }
          }
        }
      }
    }

    None
  }

  private[algorithms] def cost(startTime: Time, fromOpt: Option[Connection], through: Connection): Double = {
    val travelTime = through.departureTime.to(through.arrivalTime)
    fromOpt.fold(startTime.to(through.departureTime) + travelTime) { from =>
      val waitingTime = from.arrivalTime.to(through.departureTime)
      travelTime + waitingTime
    }
  }

}

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

  private def reconstructPath(
    predecessors: Map[Stop, Connection],
    end: Stop,
    graph: Graph,
  ): List[Connection] = {
    val path = mutable.ListBuffer.empty[Connection]
    var current = end
    var visited = Set.empty[Stop] // Track visited stops to detect cycles

    while (predecessors.contains(current) && !visited.contains(current)) {
      visited += current // Mark current stop as visited
      val parent = predecessors(current)
      current = parent.startStop
      path.prepend(parent)
    }

    // If we exited due to a cycle, return empty path (or handle error)
    if (visited.contains(current)) {
      List.empty[Connection] // or throw an exception
    } else {
      path.toList
    }
  }

  def cost(startTime: Time, fromOpt: Option[Connection], through: Connection): Double = {
    val travelTime = through.departureTime.to(through.arrivalTime)
    fromOpt.fold(startTime.to(through.departureTime) + travelTime) { from =>
      val waitingTime = from.arrivalTime.to(through.departureTime)
      travelTime + waitingTime
    }
  }

  def run(
    start: Stop,
    startTime: Time,
    end: Stop,
    graph: Graph,
  ): Option[PathFindingResult] = {
    val heuristic = lengthHeuristic

    val gScore = mutable.Map(start -> 0.0)
    val hScore = mutable.Map(start -> heuristic(start, end))
    val fScore = mutable.Map(start -> (gScore(start) + hScore(start)))
    val opened = mutable.Set(start)
    val closed = mutable.Set.empty[Stop]
    val parents = mutable.Map.empty[Stop, Connection]

    while (opened.nonEmpty) {
      val node = opened.minBy(fScore)

      if (node === end) {
        return Some(
          PathFindingResult(
            path = reconstructPath(parents.toMap, node, graph),
            cost = gScore(end),
          )
        )
      }

      opened.remove(node)
      closed.add(node)

      for (
        nextConnection <- graph(node)
                            .groupBy(_.endStop)
                            .values
                            .flatMap { connections =>
                              connections.minByOption { connection =>
                                gScore(node) + cost(startTime, parents.get(node), connection)
                              }
                            }
      ) {
        val nextNode = nextConnection.endStop
        val tentativeGScore = gScore(node) + cost(startTime, parents.get(node), nextConnection)

        if (!closed.contains(nextNode)) {
          if (!opened.contains(nextNode) || tentativeGScore < gScore.getOrElse(nextNode, Double.PositiveInfinity)) {
            parents.update(nextNode, nextConnection)
            gScore.update(nextNode, tentativeGScore)
            hScore.update(nextNode, heuristic(nextNode, end))
            fScore.update(nextNode, gScore(nextNode) + hScore(nextNode))

            if (!opened.contains(nextNode)) {
              opened.add(nextNode)
            }
          }
        }
      }
    }

    None
  }

}

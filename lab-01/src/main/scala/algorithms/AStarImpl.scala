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

    while (predecessors.contains(current)) {
      val parent = predecessors(current)
      current = parent.startStop
      path.prepend(parent)
    }

    path.toList
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

    // scalafix:off DisableSyntax.while
    while (opened.nonEmpty) {

      val node = opened.minBy(fScore)

      // scalafix:off DisableSyntax.return
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

        if (!opened.contains(nextNode) && !closed.contains(nextNode)) {

          opened.add(nextNode)
          hScore.update(nextNode, heuristic(nextNode, end))
          gScore.updateWith(nextNode)(scoreOpt =>
            scoreOpt
              .map(_ + cost(startTime, parents.get(node), nextConnection))
              .orElse(cost(startTime, parents.get(node), nextConnection).some)
          )
          fScore.updateWith(nextNode)(scoreOpt => scoreOpt.map(_ + hScore(nextNode)).orElse(hScore.get(nextNode)))
          parents.update(nextNode, nextConnection)

        } else if (gScore(nextNode) > gScore(node) + cost(startTime, parents.get(node), nextConnection)) {

          gScore.updateWith(nextNode)(scoreOpt => scoreOpt.map(_ + cost(startTime, parents.get(node), nextConnection))) // repeation extract
          fScore.updateWith(nextNode)(scoreOpt => scoreOpt.map(_ + hScore(nextNode)))
          parents.update(nextNode, nextConnection)

          if (closed.contains(nextNode)) {
            opened.add(nextNode)
            closed.remove(nextNode)
          }

        }
      }

    }

    None
  }

}

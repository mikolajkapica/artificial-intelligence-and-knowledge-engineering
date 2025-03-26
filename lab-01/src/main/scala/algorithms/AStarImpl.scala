package algorithms

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
    node: Stop,
    parents: collection.Map[Stop, Stop],
    graph: Graph,
  ): List[Connection] = {
    @tailrec
    def buildPath(current: Stop, acc: List[Connection]): List[Connection] =
      parents.get(current) match {
        case Some(parent) =>
          graph(parent).find(_.endStop === current) match {
            case Some(connection) => buildPath(parent, connection :: acc)
            case None             => throw new IllegalStateException(s"No connection found between $parent and $current")
          }
        case None         => acc
      }

    buildPath(node, List.empty)
  }

  def run(
    start: Stop,
    end: Stop,
    graph: Graph,
    cost: (Stop, Stop) => Double,
  ): Option[PathFindingResult] = {

    val neighbors = graph.apply.andThen(_.map(_.endStop))
    val cost = timeCost(graph)
    val heuristic = lengthHeuristic

    val gScore = mutable.Map(start -> 0.0)
    val hScore = mutable.Map(start -> heuristic(start, end))
    val fScore = mutable.Map(start -> (gScore(start) + hScore(start)))
    val opened = mutable.Set(start)
    val closed = mutable.Set.empty[Stop]
    val parents = mutable.Map.empty[Stop, Stop]

    // scalafix:off DisableSyntax.while
    while (opened.nonEmpty) {
      val node = opened.minBy(fScore)

      // scalafix:off DisableSyntax.return
      if (node === end)
        return Some(
          PathFindingResult(
            path = reconstructPath(node, parents, graph),
            cost = gScore(end),
          )
        )
      else {

        opened.remove(node)
        closed.add(node)

        for (nextNode <- neighbors(node))
          if (!opened.contains(nextNode) && !closed.contains(nextNode)) {
            opened.add(nextNode)
            hScore.update(nextNode, heuristic(nextNode, end))
            gScore.updateWith(nextNode)(scoreOpt => scoreOpt.map(_ + cost(node, nextNode)).orElse(Double.PositiveInfinity.some))
            fScore.updateWith(nextNode)(scoreOpt => scoreOpt.map(_ + hScore(nextNode)).orElse(Double.PositiveInfinity.some))
            parents(nextNode) = node
          } else if (gScore(nextNode) > gScore(node) + cost(node, nextNode)) {
            gScore.updateWith(nextNode)(scoreOpt => scoreOpt.map(_ + cost(node, nextNode)))
            fScore.updateWith(nextNode)(scoreOpt => scoreOpt.map(_ + hScore(nextNode)).orElse(Double.PositiveInfinity.some))
            parents.update(nextNode, node)
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

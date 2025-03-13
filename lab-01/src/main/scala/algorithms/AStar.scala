package algorithms

import scala.collection.mutable

import cats.Eq
import cats.syntax.eq._
import cats.syntax.option._

import domain.Stop

object AStar {

  private def reconstructPath[Node](node: Node, parent: collection.Map[Node, Node]): List[Node] =
    if (!parent.contains(node)) List(node)
    else node :: reconstructPath(parent(node), parent)

  def aStar[Node: Eq](
    start: Node,
    end: Node,
    neighbors: Node => Set[Node],
    cost: (Node, Node) => Double,
    heuristic: (Node, Node) => Double,
  ): Option[List[Node]] = {
    val gScore = mutable.Map(start -> 0.0)
    val hScore = mutable.Map(start -> heuristic(start, end)) // if i understand correctly
    val fScore = mutable.Map(start -> (gScore(start) + hScore(start)))
    val opened = mutable.Set(start)
    val closed = mutable.Set.empty[Node]
    val parent = mutable.Map.empty[Node, Node]

    // scalafix:off DisableSyntax.while
    while (opened.nonEmpty) {
      val node = opened.minBy(fScore)

      // scalafix:off DisableSyntax.return
      if (node === end) return reconstructPath(node, parent).some
      else {

        opened.remove(node)
        closed.add(node)

        for (nextNode <- neighbors(node))
          if (!opened.contains(nextNode) && !closed.contains(nextNode)) {
            opened.add(nextNode)
            hScore.update(nextNode, heuristic(nextNode, end))
            gScore.updateWith(nextNode)(scoreOpt => scoreOpt.map(_ + cost(node, nextNode)).orElse(Double.PositiveInfinity.some))
            fScore.updateWith(nextNode)(scoreOpt => scoreOpt.map(_ + hScore(nextNode)).orElse(Double.PositiveInfinity.some))
            parent(nextNode) = node
          } else if (gScore(nextNode) > gScore(node) + cost(node, nextNode)) {
            gScore.updateWith(nextNode)(scoreOpt => scoreOpt.map(_ + cost(node, nextNode)))
            fScore.updateWith(nextNode)(scoreOpt => scoreOpt.map(_ + hScore(nextNode)).orElse(Double.PositiveInfinity.some))
            parent.update(nextNode, nextNode)
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

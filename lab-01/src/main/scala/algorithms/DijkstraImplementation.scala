package algorithms

import cats.Eq
import cats.implicits.catsSyntaxEq
import domain.{Connection, Graph, Stop}

import scala.collection.mutable

object DijkstraImplementation {

  def run(
    start: Stop,
    end: Stop,
    graph: Graph,
  ): Option[PathFindingResult] = {
    val vertices = graph.keys.toSet
    val cost = timeCost(graph)

    val p = mutable.Map.empty[Stop, Stop]
    val d = vertices.map(node => (node, if node === start then 0 else Double.PositiveInfinity)).to(collection.mutable.Map)
    val Q = vertices.to(collection.mutable.Set)

    // scalafix:off DisableSyntax.while
    while (Q.nonEmpty) {
      val u = Q.minBy(d)
      Q.remove(u)

      // idk?
      if (u === end) {
        return Some(reconstructPath(p.toMap, end))
      }

      for {
        v <- vertices if d(v) > d(u) + cost(u, v)
      } {
        d(v) = d(u) + cost(u, v)
        p(v) = u
      }
    }
    None
  }

  private def reconstructPath[Node](
    predecessors: Map[Node, Node],
    end: Node,
  ): List[Node] = {
    val path = mutable.ListBuffer.empty[Node]
    var current = end
    while (predecessors.contains(current)) {
      path.prepend(current)
      current = predecessors(current)
    }
    path.prepend(current).toList
  }

}

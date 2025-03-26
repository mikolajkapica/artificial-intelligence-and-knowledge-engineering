package algorithms

import cats.implicits.catsSyntaxEq
import domain.Connection
import domain.Graph
import domain.Stop

import scala.collection.mutable

object DijkstraImpl {

  def run(
    start: Stop,
    end: Stop,
    graph: Graph,
    cost: (Stop, Stop) => Double,
  ): Option[PathFindingResult] = {
    val vertices = graph.keys.toSet

    val p = mutable.Map.empty[Stop, Stop]
    val d = vertices.map(node => (node, if (node === start) 0 else Double.PositiveInfinity)).to(collection.mutable.Map)
    val Q = vertices.to(collection.mutable.Set)

    // scalafix:off DisableSyntax.while
    while (Q.nonEmpty) {
      val u = Q.minBy(d)
      Q.remove(u)

      if (u === end) {
        return Some(
          PathFindingResult(
            path = reconstructPath(p.toMap, end, graph),
            cost = d(end),
          )
        )
      }

      for {
        v <- graph(u).map(_.endStop) if d(v) > d(u) + cost(u, v)
      } {
        d(v) = d(u) + cost(u, v)
        p(v) = u
      }
    }
    None
  }

  private def reconstructPath(
    predecessors: Map[Stop, Stop],
    end: Stop,
    graph: Graph,
  ): List[Connection] = {
    val path = mutable.ListBuffer.empty[Connection]
    var current = end

    while (predecessors.contains(current)) {
      val parent = predecessors(current)
      graph(parent).find(_.endStop === current) match {
        case Some(connection) => path.prepend(connection)
        case None             => throw new IllegalStateException(s"No connection found between $parent and $current")
      }
      current = parent
    }

    path.toList
  }

}

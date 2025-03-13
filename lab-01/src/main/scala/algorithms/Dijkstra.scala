package algorithms

import scala.collection.mutable

import cats.Eq
import cats.implicits.catsSyntaxEq

object Dijkstra {

  def dijkstra[Node: Eq](
    start: Node,
    end: Node,
    vertices: collection.Set[Node],
    w: (Node, Node) => Double,
  ): (Map[Node, Double], Map[Node, Node]) = {
    val p = mutable.Map.empty[Node, Node]
    val d = vertices.map(node => (node, if node === start then 0 else Double.PositiveInfinity)).to(collection.mutable.Map)
    val Q = vertices.to(collection.mutable.Set)

    // scalafix:off DisableSyntax.while
    while (Q.nonEmpty) {
      val u = Q.minBy(d)
      Q.remove(u)
      for {
        v <- vertices if d(v) > d(u) + w(u, v)
      } {
        d(v) = d(u) + w(u, v)
        p(v) = u
      }
    }
    (d.toMap, p.toMap)
  }

}

package algorithms

import algorithms.utils.CostFunctions.CostFunction
import algorithms.utils.PathFindingResult
import algorithms.utils.reconstructPath
import cats.implicits.catsSyntaxEq
import domain.Connection
import domain.Graph
import domain.Stop
import domain.Time

import scala.collection.mutable

object DijkstraImpl {

  def run(
    start: Stop,
    startTime: Time,
    end: Stop,
    graph: Graph,
    cost: CostFunction,
  ): Option[PathFindingResult] = {
    val vertices = graph.keys.toSet

    val p = mutable.Map.empty[Stop, Connection]
    val d = vertices.map(node => (node, if (node === start) 0 else Double.PositiveInfinity)).to(collection.mutable.Map)
    val Q = vertices.to(collection.mutable.Set)

    // scalafix:off DisableSyntax.while
    while (Q.nonEmpty) {
      val u = Q.minBy(d)
      Q.remove(u)

      if (u === end) {
        return if (!p.contains(u)) None
        else Some(PathFindingResult(path = reconstructPath(p.toMap, end), cost = d(end)))
      }

      for {
        uConnection <-
          graph(u)
            .groupBy(_.endStop)
            .values
            .flatMap(_.minByOption(connectionCost => d(u) + cost(startTime, p.get(u), connectionCost)))
        v = uConnection.endStop
        connectionCost = cost(startTime, p.get(u), uConnection)
        if d(v) > d(u) + connectionCost
      } {
        d(v) = d(u) + connectionCost
        p(v) = uConnection
      }

    }

    None
  }

}

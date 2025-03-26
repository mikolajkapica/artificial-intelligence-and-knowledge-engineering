package algorithms

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
//    cost: CostFunction,
  ): Option[PathFindingResult] = {
    val vertices = graph.keys.toSet

    val p = mutable.Map.empty[Stop, Connection] // parent
    val d = vertices.map(node => (node, if (node === start) 0 else Double.PositiveInfinity)).to(collection.mutable.Map) // distance
    val Q = vertices.to(collection.mutable.Set) // unvisited vertices

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
        uConnection <-
          graph(u).groupBy(_.endStop).values.flatMap(_.minByOption(connectionCost => d(u) + cost(startTime, p.get(u), connectionCost)))
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

}

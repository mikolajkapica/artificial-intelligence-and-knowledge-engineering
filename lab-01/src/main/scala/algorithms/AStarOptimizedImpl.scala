package algorithms

import utils.CostFunctions.CostFunction
import domain.Connection
import domain.Graph
import domain.Stop
import domain.Time
import utils.{PathFindingResult, reconstructPath}

import scala.collection.mutable

object AStarOptimizedImpl {

  def run(
    start: Stop,
    startTime: Time,
    end: Stop,
    graph: Graph,
    costFunction: CostFunction,
    heuristic: (Stop, Stop) => Double,
  ): Option[PathFindingResult] = {

    val gScore = mutable.Map(start -> 0.0).withDefaultValue(Double.PositiveInfinity)
    val hScore = mutable.Map(start -> heuristic(start, end))
    val fScore = mutable.Map(start -> (gScore(start) + hScore(start))).withDefaultValue(Double.PositiveInfinity)

    val opened = mutable.PriorityQueue(start)(Ordering.by(fScore(_)).reverse)

    val closed = mutable.Set.empty[Stop]
    val predecessors = mutable.Map.empty[Stop, Connection]

    while (opened.nonEmpty) {
      val currentNode = opened.dequeue()

      if (!closed.contains(currentNode)) {

        closed.add(currentNode)

        if (currentNode == end) {
          return Some(
            PathFindingResult(
              path = reconstructPath(predecessors.toMap, end),
              cost = gScore(end),
            )
          )
        }

        for (connection <- graph.getOrElse(currentNode, List.empty)) {
          val neighbor = connection.endStop

          val tentativeGScore = gScore(currentNode) + costFunction(
            startTime,
            predecessors.get(currentNode),
            connection,
          )

          if (tentativeGScore < gScore(neighbor)) {
            predecessors.update(neighbor, connection)
            gScore.update(neighbor, tentativeGScore)
            hScore.update(neighbor, heuristic(neighbor, end))
            fScore.update(neighbor, gScore(neighbor) + hScore(neighbor))

            opened.enqueue(neighbor)
          }
        }
      }
    }

    None
  }

}

package algorithms

import utils.CostFunctions.CostFunction
import domain.Connection
import domain.Graph
import domain.Stop
import domain.Time
import utils.PathFindingResult
import utils.reconstructPath

import scala.collection.mutable

object AStarImpl {

  def run(
    start: Stop,
    startTime: Time,
    end: Stop,
    graph: Graph,
    costFunction: CostFunction,
    heuristic: (Stop, Stop) => Double,
  ): Option[PathFindingResult] = {

    val gScore = mutable.Map(start -> 0.0).withDefaultValue(Double.PositiveInfinity)
    val hScore = mutable.Map(start -> heuristic(start, end)).withDefaultValue(Double.PositiveInfinity)
    val fScore = mutable.Map(start -> (gScore(start) + hScore(start))).withDefaultValue(Double.PositiveInfinity)
    val opened = mutable.Set(start)
    val closed = mutable.Set.empty[Stop]

    val predecessors = mutable.Map.empty[Stop, Connection]

    while (opened.nonEmpty) {
      var currentNode: Stop = null
      var currentCost = Double.PositiveInfinity

      for (node <- opened)
        if (fScore(node) < currentCost) {
          currentNode = node
          currentCost = fScore(node)
        }

      if (currentNode == end) {
        return Some(
          PathFindingResult(
            path = reconstructPath(predecessors.toMap, end),
            cost = gScore(end),
          )
        )
      }

      opened.remove(currentNode)
      closed.add(currentNode)

      for (connection <- graph(currentNode)) {
        val neighbor = connection.endStop
        val tentativeGScore = gScore(currentNode) + costFunction(startTime, predecessors.get(currentNode), connection)

        if (!opened.contains(neighbor) && !closed.contains(neighbor)) {
          opened.add(neighbor)
          hScore.update(neighbor, heuristic(neighbor, end))
          gScore.update(neighbor, tentativeGScore)
          fScore.update(neighbor, gScore(neighbor) + hScore(neighbor))

          predecessors.update(neighbor, connection)

        } else {
          if (tentativeGScore < gScore(neighbor)) {
            gScore.update(neighbor, tentativeGScore)
            fScore.update(neighbor, gScore(neighbor) + hScore(neighbor))

            predecessors.update(neighbor, connection)

            if (closed.contains(neighbor)) {
              opened.add(neighbor)
              closed.remove(neighbor)
            }
          }
        }
      }
    }

    None
  }

}

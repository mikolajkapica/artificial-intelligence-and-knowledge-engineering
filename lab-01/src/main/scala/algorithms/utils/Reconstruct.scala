package algorithms.utils

import domain.Connection
import domain.Stop

import scala.collection.mutable

def reconstructPath(
  predecessors: Map[Stop, Connection],
  end: Stop,
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

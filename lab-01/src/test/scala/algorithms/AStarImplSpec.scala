package algorithms

import domain.*
import munit.FunSuite

import scala.collection.immutable.Map

class AStarImplSpec extends FunSuite {

  def stop(name: String): Stop = Stop(name, WGS84(0, 0))

  def time(h: Int, m: Int = 0): Time = Time(h, m, 0)

  def conn(from: Stop, to: Stop, dep: Time, arr: Time, line: String = "1"): Connection = Connection("TEST", line, dep, arr, from, to)

  private val A = stop("A")
  private val B = stop("B")
  private val C = stop("C")
  private val D = stop("D")

  test("reconstructPath builds correct path from predecessors") {
    val predecessors = Map(
      B -> conn(A, B, time(8), time(9)),
      C -> conn(B, C, time(9, 30), time(10, 30)),
    )

    val path = AStarImpl.reconstructPath(predecessors, C)

    assertEquals(path.size, 2)
    assertEquals(path.head.startStop.name, "A")
    assertEquals(path.last.endStop.name, "C")
    assertEquals(path.map(_.line), List("1", "1"))
  }

  test("selects fastest path considering transfer times") {
    val graph: Graph = Map(
      A -> Set(
        conn(A, B, time(8), time(9)), // Fast option
        conn(A, D, time(8), time(10)), // Slow option
      ),
      B -> Set(conn(B, C, time(9, 30), time(10, 15))),
      D -> Set(conn(D, C, time(10, 30), time(11, 45))),
      C -> Set.empty,
    )

    val result = AStarImpl.run(A, time(8), C, graph).get

    // Should choose A->B->C (1h + 45m) over A->D->C (2h + 1h15m)
    assertEquals(result.path.map(_.endStop.name), List("B", "C"))
  }

  test("prefers direct connection over transfer when faster") {
    val graph: Graph = Map(
      A -> Set(
        conn(A, B, time(8), time(9)),
        conn(A, C, time(8), time(9, 15)),
      ),
      B -> Set(conn(B, C, time(9, 30), time(9, 45))),
      C -> Set.empty,
    )

    val result = AStarImpl.run(A, time(8), C, graph).get
    assertEquals(result.path.size, 1)
    assertEquals(result.path.head.line, "1")
  }

}

package algorithms

import domain.*
import munit.FunSuite
import scala.collection.immutable.Map

class DijkstraImplSpec extends FunSuite {

  // Helper methods
  def stop(name: String): Stop = Stop(name, WGS84(0, 0))

  def time(h: Int, m: Int = 0): Time = Time(h, m, 0)

  def conn(from: Stop, to: Stop, dep: Time, arr: Time, line: String = "1"): Connection =
    Connection("TEST", line, dep, arr, from, to)

  // Test stops
  private val A = stop("A")
  private val B = stop("B")
  private val C = stop("C")
  private val D = stop("D")
  private val E = stop("E")

  test("reconstructPath builds correct path from predecessors") {
    val predecessors = Map(
      B -> conn(A, B, time(8), time(9)),
      C -> conn(B, C, time(9, 30), time(10, 30)),
      D -> conn(C, D, time(10, 45), time(11, 30))
    )

    val path = DijkstraImpl.reconstructPath(predecessors, D, Map.empty)

    assertEquals(path.size, 3)
    assertEquals(path.head.startStop.name, "A")
    assertEquals(path.last.endStop.name, "D")
    assertEquals(path.map(_.line), List("1", "1", "1"))
  }

  test("finds shortest path considering both travel and wait times") {
    val graph: Graph = Map(
      A -> Set(
        conn(A, B, time(8), time(9)), // Fast path
        conn(A, D, time(8), time(10)) // Slower path
      ),
      B -> Set(conn(B, C, time(9, 30), time(10, 15))),
      D -> Set(conn(D, C, time(10, 30), time(11, 45))),
      C -> Set.empty
    )

    val result = DijkstraImpl.run(A, time(8), C, graph).get

    // Should choose A->B->C (1h + 45m) over A->D->C (2h + 1h15m)
    assertEquals(result.path.map(_.endStop.name), List("B", "C"))
  }

  test("prefers direct connection when optimal") {
    val graph: Graph = Map(
      A -> Set(
        conn(A, C, time(8), time(9, 15)), // Direct connection
        conn(A, B, time(8), time(8, 45)) // Transfer option
      ),
      B -> Set(conn(B, C, time(9), time(9, 30))),
      C -> Set.empty
    )

    val result = DijkstraImpl.run(A, time(8), C, graph).get
    assertEquals(result.path.size, 1) // Should take direct connection
  }

  test("returns None when no path exists") {
    val graph: Graph = Map(
      A -> Set(conn(A, B, time(8), time(9))),
      B -> Set.empty,
      C -> Set.empty
    )

    val result = DijkstraImpl.run(A, time(8), C, graph)
    assertEquals(result, None)
  }

  test("handles complex graph with multiple options") {
    val graph: Graph = Map(
      A -> Set(
        conn(A, B, time(8), time(9)),
        conn(A, D, time(8), time(8, 30))
      ),
      B -> Set(
        conn(B, C, time(9, 30), time(10)),
        conn(B, E, time(9, 15), time(10, 15))
      ),
      D -> Set(
        conn(D, E, time(9), time(10)),
        conn(D, C, time(8, 45), time(10))
      ),
      E -> Set(conn(E, C, time(10, 30), time(11))),
      C -> Set.empty
    )

    val result = DijkstraImpl.run(A, time(8), C, graph).get
  }
}

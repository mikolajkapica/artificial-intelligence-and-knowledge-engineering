package algorithms

import utils.CostFunctions.getCostFunction
import domain.*
import munit.FunSuite
import utils.Optimization

import scala.collection.immutable.Map

class DijkstraImplSpec extends FunSuite {

  def stop(name: String): Stop = Stop(name, WGS84(0, 0))

  def time(h: Int, m: Int = 0): Time = Time(h, m, 0)

  def conn(from: Stop, to: Stop, dep: Time, arr: Time, line: String = "1"): Connection =
    Connection("TEST", line, dep, arr, from, to)

  private val A = stop("A")
  private val B = stop("B")
  private val C = stop("C")
  private val D = stop("D")
  private val E = stop("E")

  private val costFunction = getCostFunction(Optimization.Time)

  test("finds shortest path considering both travel and wait times") {
    val graph: Graph = Map(
      A -> Set(
        conn(A, B, time(8), time(9)),
        conn(A, D, time(8), time(10)),
      ),
      B -> Set(conn(B, C, time(9, 30), time(10, 15))),
      D -> Set(conn(D, C, time(10, 30), time(11, 45))),
      C -> Set.empty,
    )

    val result = DijkstraImpl.run(A, time(8), C, graph, costFunction).get

    // Should choose A->B->C (1h + 45m) over A->D->C (2h + 1h15m)
    assertEquals(
      result.path,
      List(
        conn(A, B, time(8), time(9)),
        conn(B, C, time(9, 30), time(10, 15)),
      ),
    )
  }

  test("prefers direct connection when optimal") {
    val graph: Graph = Map(
      A -> Set(
        conn(A, C, time(8), time(9, 15)),
        conn(A, B, time(8), time(8, 45)),
      ),
      B -> Set(conn(B, C, time(9), time(9, 30))),
      C -> Set.empty,
    )

    val result = DijkstraImpl.run(A, time(8), C, graph, costFunction).get
    assertEquals(
      result.path,
      List(
        conn(A, C, time(8), time(9, 15))
      ),
    )
  }

  test("returns None when no path exists") {
    val graph: Graph = Map(
      A -> Set(conn(A, B, time(8), time(9))),
      B -> Set.empty,
      C -> Set.empty,
    )

    val result = DijkstraImpl.run(A, time(8), C, graph, costFunction)
    assertEquals(result, None)
  }

  test("handles complex graph with multiple options") {
    val graph: Graph = Map(
      A -> Set(
        conn(A, B, time(8), time(9)),
        conn(A, D, time(8), time(8, 30)),
      ),
      B -> Set(
        conn(B, C, time(9, 30), time(10)),
        conn(B, E, time(9, 15), time(10, 15)),
      ),
      D -> Set(
        conn(D, E, time(9), time(10)),
        conn(D, C, time(8, 45), time(10)),
      ),
      E -> Set(conn(E, C, time(10, 30), time(11))),
      C -> Set.empty,
    )

    val result = DijkstraImpl.run(A, time(8), C, graph, costFunction).get
    assertEquals(
      result.path,
      List(
        conn(A, D, time(8), time(8, 30)),
        conn(D, C, time(8, 45), time(10)),
      ),
    )
  }
}

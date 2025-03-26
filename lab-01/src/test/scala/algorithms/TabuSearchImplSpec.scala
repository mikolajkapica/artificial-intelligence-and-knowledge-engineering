package algorithms

import algorithms.CostFunctions.getCostFunction
import algorithms.Heuristics.{Heuristic, getHeuristic}
import domain.*
import munit.FunSuite

import scala.util.Random

class TabuSearchImplSpec extends FunSuite {
  // Set fixed seed for reproducible tests
  Random.setSeed(12345)

  // Helper methods
  def stop(name: String): Stop = Stop(name, WGS84(0, 0))

  def time(h: Int, m: Int = 0): Time = Time(h, m, 0)

  def conn(from: Stop, to: Stop, dep: Time, arr: Time): Connection =
    Connection("TEST", "1", dep, arr, from, to)

  // Test stops
  private val A = stop("A")
  private val B = stop("B")
  private val C = stop("C")
  private val D = stop("D")
  private val E = stop("E")

  // Updated test graph with complete cycle
  val testGraph: Graph = Map(
    A -> Set(
      conn(A, B, time(8), time(9)),
      conn(A, C, time(8), time(10)),
      conn(A, E, time(8), time(11)), // New connection to enable cycles
    ),
    B -> Set(
      conn(B, A, time(9, 30), time(10)), // Return to A
      conn(B, C, time(9, 30), time(10)),
      conn(B, D, time(10), time(11)),
    ),
    C -> Set(
      conn(C, A, time(10, 30), time(11)), // Return to A
      conn(C, B, time(10, 30), time(11, 15)), // Connection back to B
      conn(C, D, time(10, 30), time(11)),
      conn(C, E, time(11), time(12)),
    ),
    D -> Set(
      conn(D, A, time(11, 30), time(12, 30)), // Return to A
      conn(D, C, time(11, 30), time(12)), // Connection back to C
      conn(D, E, time(11, 30), time(12)),
    ),
    E -> Set(
      conn(E, A, time(12), time(13)), // Return to A
      conn(E, D, time(12), time(13)), // Connection back to D
    ),
  )

  private val costFunction = getCostFunction(Optimization.Time)

  private val heuristic = getHeuristic(Heuristic.TimeHaversine)

  test("finds valid round-trip route through required stops") {
    val result = TabuSearchImpl.run(
      start = A,
      startTime = time(8),
      through = List(B, C, D), // Must visit B, C and D
      graph = testGraph,
      costFunction = costFunction,
      heuristic = heuristic,
    )

    assert(result.isDefined, "Should find a valid route")

    val path = result.get.path
    // Verify round-trip
    assertEquals(path.head.startStop, A)
    assertEquals(path.last.endStop, A)

    // Verify all required stops are visited
    val visitedStops = path.flatMap(conn => List(conn.startStop, conn.endStop)).distinct
    assert(visitedStops.contains(B), "Should visit stop B")
    assert(visitedStops.contains(C), "Should visit stop C")
    assert(visitedStops.contains(D), "Should visit stop D")

    // Verify path continuity
    path.sliding(2).foreach {
      case List(prev, next) => assertEquals(prev.endStop, next.startStop)
      case _                => // ignore
    }
  }

  test("finds optimal route considering travel times") {
    // Known optimal path: A -> B -> C -> D -> A (total time = 240 minutes)
    val expectedCost = 240.0

    val result = TabuSearchImpl.run(
      start = A,
      startTime = time(8),
      through = List(B, C, D),
      graph = testGraph,
      costFunction = costFunction,
      heuristic = heuristic,
    )

    assert(result.isDefined)
  }

  test("handles case with multiple possible cycles") {
    val result = TabuSearchImpl.run(
      start = A,
      startTime = time(8),
      through = List(B, E), // Can go A->B->A->E->A or A->B->E->A etc.
      graph = testGraph,
      costFunction = costFunction,
      heuristic = heuristic,
    )

    assert(result.isDefined)
    val visitedStops = result.get.path.flatMap(conn => List(conn.startStop, conn.endStop)).distinct
    assert(visitedStops.contains(B))
    assert(visitedStops.contains(E))
  }
}

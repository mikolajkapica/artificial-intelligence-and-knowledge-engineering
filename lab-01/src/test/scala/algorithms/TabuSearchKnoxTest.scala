package algorithms

import munit.FunSuite
import domain.Stop
import domain.Graph
import domain.Connection
import domain.Time
import domain.WGS84

class TabuSearchKnoxTest extends FunSuite {
  val stopA = Stop("A", WGS84(0.0, 0.0))
  val stopB = Stop("B", WGS84(1.0, 1.0))
  val stopC = Stop("C", WGS84(2.0, 2.0))
  val stopD = Stop("D", WGS84(3.0, 3.0))
  val stopE = Stop("E", WGS84(4.0, 4.0))

  val connections: List[Connection] = List(
    Connection("Test", "1", Time(10, 0, 0), Time(10, 5, 0), stopA, stopB),
    Connection("Test", "1", Time(10, 0, 0), Time(10, 7, 0), stopA, stopC),
    Connection("Test", "1", Time(10, 0, 0), Time(10, 12, 0), stopA, stopD),
    Connection("Test", "1", Time(10, 0, 0), Time(10, 10, 0), stopA, stopE),
    Connection("Test", "1", Time(10, 6, 0), Time(10, 9, 0), stopB, stopC),
    Connection("Test", "1", Time(10, 6, 0), Time(10, 14, 0), stopB, stopD),
    Connection("Test", "1", Time(10, 6, 0), Time(10, 12, 0), stopB, stopE),
    Connection("Test", "1", Time(10, 6, 0), Time(10, 13, 0), stopB, stopA),
    Connection("Test", "1", Time(10, 10, 0), Time(10, 14, 0), stopC, stopD),
    Connection("Test", "1", Time(10, 10, 0), Time(10, 19, 0), stopC, stopE),
    Connection("Test", "1", Time(10, 10, 0), Time(10, 16, 0), stopC, stopA),
    Connection("Test", "1", Time(10, 10, 0), Time(10, 13, 0), stopC, stopB),
    Connection("Test", "1", Time(10, 15, 0), Time(10, 17, 0), stopD, stopE),
    Connection("Test", "1", Time(10, 15, 0), Time(10, 25, 0), stopD, stopA),
    Connection("Test", "1", Time(10, 15, 0), Time(10, 23, 0), stopD, stopB),
    Connection("Test", "1", Time(10, 15, 0), Time(10, 22, 0), stopD, stopC),
    Connection("Test", "1", Time(10, 20, 0), Time(10, 28, 0), stopE, stopA),
    Connection("Test", "1", Time(10, 20, 0), Time(10, 27, 0), stopE, stopB),
    Connection("Test", "1", Time(10, 20, 0), Time(10, 30, 0), stopE, stopC),
    Connection("Test", "1", Time(10, 20, 0), Time(10, 25, 0), stopE, stopD),
  )

  val testGraph: Graph =
    connections.groupBy(_.startStop).view.mapValues(_.toSet).toMap

  val through: List[Stop] = List(stopB, stopC, stopD, stopE)

  def costFunction(a: Stop, b: Stop): Double =
    testGraph
      .get(a)
      .flatMap(_.find(_.endStop == b))
      .map { conn =>
        val depart = conn.departureTime.hour * 60 + conn.departureTime.minute
        val arrive = conn.arrivalTime.hour * 60 + conn.arrivalTime.minute
        (arrive - depart).toDouble
      }
      .getOrElse(Double.MaxValue)

  test("valid cycle route with more data") {
    val resultOpt = TabuSearchKnox.run(stopA, through, testGraph, costFunction)
    assert(resultOpt.isDefined)
    val result = resultOpt.get
    val firstStop = result.path.head.startStop
    val lastStop = result.path.last.endStop
    assertEquals(firstStop, stopA)
    assertEquals(lastStop, stopA)
    val routeStops = (result.path.map(_.startStop) :+ lastStop).toSet
    through.foreach(stop => assert(routeStops.contains(stop)))
  }

  test("correct total cost with more data") {
    val resultOpt = TabuSearchKnox.run(stopA, through, testGraph, costFunction)
    assert(resultOpt.isDefined)
    val result = resultOpt.get
    val computedCost =
      result.path.map(conn => costFunction(conn.startStop, conn.endStop)).sum
    assertEquals(result.cost, computedCost)
  }
}

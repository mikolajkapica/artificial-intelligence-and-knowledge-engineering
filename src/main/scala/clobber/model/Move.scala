package clobber.model

// It needs Position, which is in the same package, so no explicit import needed here.
// case class Position(row: Int, col: Int) // Already in Position.scala

case class Move(from: Position, to: Position) {
  override def toString: String = s"(${from.row},${from.col})->(${to.row},${to.col})"
}

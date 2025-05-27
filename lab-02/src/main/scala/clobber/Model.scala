package clobber

import cats.syntax.traverse._
import cats.instances.list._
import cats.instances.vector._

enum Square:
  case B, W, Empty

  def stringRep: String = this match
    case Square.B     => "B"
    case Square.W     => "W"
    case Square.Empty => "_"

object Square:
  def fromChar(char: Char): Option[Square] = char match
    case 'B' => Some(Square.B)
    case 'W' => Some(Square.W)
    case '_' => Some(Square.Empty)
    case _   => None

enum Player:
  case Black, White

  def other: Player = this match
    case Black => White
    case White => Black

  def shortName: String = this match
    case Black => "B"
    case White => "W"

  def square: Square = this match
    case Black => Square.B
    case White => Square.W

case class Position(row: Int, col: Int) // TODO: Add type constraints

case class Move(from: Position, to: Position):
  override def toString: String = s"(${from.row},${from.col})->(${to.row},${to.col})"

case class Board(grid: Vector[Vector[Square]], numRows: Int, numCols: Int) {
  def getSquare(pos: Position): Option[Square] =
    if (isOnBoard(pos)) Some(grid(pos.row)(pos.col))
    else None

  def isOnBoard(pos: Position): Boolean =
    pos.row >= 0
      && pos.row < numRows
      && pos.col >= 0
      && pos.col < numCols

  def applyMove(move: Move, player: Player): Board = {
    val gridWithSourcePieceRemoved = grid.updated(
      move.from.row,
      grid(move.from.row).updated(move.from.col, Square.Empty)
    )

    val gridWithTargetPieceTaken = gridWithSourcePieceRemoved.updated(
      move.to.row,
      gridWithSourcePieceRemoved(move.to.row).updated(move.to.col, player.square)
    )

    this.copy(grid = gridWithTargetPieceTaken)
  }

  override def toString: String =
    grid.map(row => row.map(sq => sq.stringRep).mkString(" ")).mkString("\n")
}

object Board {
  def parse(config: List[String]): Either[String, Board] = for {
    expectedNumCols <- config.headOption.map(_.split(' ').length).toRight(
      "Board configuration must have consistent column counts across all rows."
    )
    numRows = config.size
    grid <- config.toVector.zipWithIndex.traverse { case (rowStr, rowIndex) =>
      val parts = rowStr.split(' ')
      if (parts.length != expectedNumCols) {
        Left(
          s"Inconsistent column count at row ${rowIndex + 1}. Expected $expectedNumCols columns, but found ${parts.length}. Row: '$rowStr'"
        )
      } else {
        parts.toVector.zipWithIndex.traverse { case (squareStr, colIndex) =>
          if (squareStr.length == 1) {
            Square.fromChar(squareStr.charAt(0)).toRight(
              s"Invalid character '${squareStr.charAt(0)}' at row ${rowIndex + 1}, column ${colIndex + 1} (part: '$squareStr')."
            )
          } else {
            Left(
              s"Invalid square string '$squareStr' at row ${rowIndex + 1}, column ${colIndex + 1}. Expected a single character representation."
            )
          }
        }
      }
    }
  } yield Board(grid, numRows, expectedNumCols)
}

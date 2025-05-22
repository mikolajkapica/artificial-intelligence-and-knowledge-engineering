package clobber.model

case class Board(grid: Vector[Vector[Square]], numRows: Int, numCols: Int) {

  def getSquare(pos: Position): Option[Square] = {
    if (isOnBoard(pos)) {
      Some(grid(pos.row)(pos.col))
    } else {
      None
    }
  }

  def isOnBoard(pos: Position): Boolean = {
    pos.row >= 0 && pos.row < numRows && pos.col >= 0 && pos.col < numCols
  }

  def applyMove(move: Move, player: Player): Board = {
    val newGrid = grid
      .updated(move.from.row, grid(move.from.row).updated(move.from.col, Square.Empty))
      .updated(move.to.row, grid(move.to.row).updated(move.to.col, player match {
        case Player.Black => Square.B
        case Player.White => Square.W
      }))
    this.copy(grid = newGrid)
  }

  override def toString: String = {
    grid.map { row =>
      row.map(_.stringRep).mkString(" ")
    }.mkString("\n")
  }
}

object Board {
  def fromString(config: List[String]): Either[String, Board] = {
    if (config.isEmpty) {
      Left("Empty board configuration")
    } else {
      // Cats imports for traverse
      import cats.syntax.traverse._
      import cats.instances.list._ // for List[String].traverse
      import cats.instances.vector._ // for Vector[String].traverse (parts.toVector.traverse)

      val numRows = config.size

      // Determine expected number of columns from the first row.
      // If the first row is, e.g. "B W _", expectedNumCols is 3.
      // If the first row is "", split(' ') yields Array(""), so expectedNumCols is 1.
      // This will be handled by the squareStr.length != 1 check later, making "" an invalid square.
      // If the first row is "  " (two spaces), split(' ') yields Array("", "", ""), so expectedNumCols is 3.
      // These empty strings will also be caught by squareStr.length != 1.
      val expectedNumCols = config.head.split(' ').length

      // Traverse each row string in the configuration.
      val parsedGridEither: Either[String, List[Vector[Square]]] =
        config.zipWithIndex.traverse { case (rowStr, rowIndex) =>
          val parts = rowStr.split(' ')
          if (parts.length != expectedNumCols) {
            Left(s"Inconsistent column count at row ${rowIndex + 1}. Expected $expectedNumCols columns, but found ${parts.length}. Row: '$rowStr'")
          } else {
            // Traverse each part (potential square string) in the current row.
            parts.toVector.zipWithIndex.traverse { case (squareStr, colIndex) =>
              if (squareStr.length == 1) {
                Square.fromString(squareStr.charAt(0))
                  .toRight(s"Invalid character '${squareStr.charAt(0)}' at row ${rowIndex + 1}, column ${colIndex + 1} (part: '$squareStr').")
              } else {
                // This handles empty strings from multiple spaces (e.g. "B  W" -> "" part)
                // or invalid multi-character strings (e.g. "BB").
                Left(s"Invalid square string '$squareStr' at row ${rowIndex + 1}, column ${colIndex + 1}. Expected a single character representation.")
              }
            }
          }
        }

      // If parsing all rows was successful, create the Board.
      parsedGridEither match {
        case Right(gridAsList) =>
          val finalGrid = gridAsList.map(_.toVector).toVector
          // The case of empty config resulting in expectedNumCols = 0 is handled by initial check.
          // If config was List(""), expectedNumCols = 1. The part "" would fail squareStr.length == 1.
          // If config was List("  "), expectedNumCols = 3. Parts "", "", "" would fail.
          // So, if we reach here with a Right, expectedNumCols should be positive if rows were non-empty.
          // It's possible for a row to be " " which means expectedNumCols = 2 (parts "", ""), which would fail.
          // This logic seems okay. numCols should be derived from expectedNumCols.
          Right(Board(finalGrid, numRows, expectedNumCols))
        case Left(errorMsg) => Left(errorMsg)
      }
    }
  }
}

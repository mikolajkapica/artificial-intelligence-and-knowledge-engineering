package clobber.ai

import clobber.{Board, Player, Square}
import clobber.generateMoves
import Helpers.*

sealed trait Heuristic:
  def evaluate(board: Board, forPlayer: Player): Double

  override def toString: String = this match
    case MobilityHeuristic   => "mobility"
    case PieceCountHeuristic => "piececount"
    case PositionalHeuristic => "positional"
    case NewTrends           => "newtrends"
    case OpponentPenalty     => "opponentpenalty"
    case GroupingLonely      => "groupinglonely"

object Heuristic:
  def parse(name: String): Option[Heuristic] = name.toLowerCase match
    case "mobility"        => Some(MobilityHeuristic)
    case "piececount"      => Some(PieceCountHeuristic)
    case "positional"      => Some(PositionalHeuristic)
    case "newtrends"       => Some(NewTrends)
    case "opponentpenalty" => Some(OpponentPenalty)
    case "groupinglonely"  => Some(GroupingLonely)
    case _                 => None

/** Heurystyka oparta na mobilności, czyli różnicy liczby możliwych ruchów gracza i przeciwnika */
object MobilityHeuristic extends Heuristic:
  override def evaluate(board: Board, forPlayer: Player): Double =
    val myMoves = generateMoves(board, forPlayer).size.toDouble
    val opponentMoves = generateMoves(board, forPlayer.other).size.toDouble
    myMoves - opponentMoves

/** Heurystyka oparta na różnicy liczby bierek gracza i przeciwnika */
object PieceCountHeuristic extends Heuristic:
  override def evaluate(board: Board, forPlayer: Player): Double =
    var myPieceCount = 0
    var opponentPieceCount = 0

    val myPlayerSquare = forPlayer.square
    val opponentPlayerSquare = forPlayer.other.square

    for (r <- 0 until board.numRows; c <- 0 until board.numCols) {
      board.grid(r)(c) match
        case s if s == myPlayerSquare       => myPieceCount += 1
        case s if s == opponentPlayerSquare => opponentPieceCount += 1
        case _                              => // Empty square
    }

    (myPieceCount - opponentPieceCount).toDouble

/** Heurystyka oparta na pozycji bierek na planszy. Preferowane są pola różne od krawędzi i rogu planszy.
  */
object PositionalHeuristic extends Heuristic:
  private def getPositionScore(row: Int, col: Int, numRows: Int, numCols: Int): Double =
    val isCorner = (row == 0 || row == numRows - 1) && (col == 0 || col == numCols - 1)
    val isEdge = (row == 0 || row == numRows - 1 || col == 0 || col == numCols - 1) && !isCorner

    if (isCorner) 0.5
    else if (isEdge) 1.0
    else 2.0

  override def evaluate(board: Board, forPlayer: Player): Double =
    var myPositionalScore = 0.0
    var opponentPositionalScore = 0.0

    val myPlayerSquare = forPlayer.square
    val opponentPlayerSquare = forPlayer.other.square

    for (r <- 0 until board.numRows; c <- 0 until board.numCols) {
      val score = getPositionScore(r, c, board.numRows, board.numCols)
      board.grid(r)(c) match
        case s if s == myPlayerSquare       => myPositionalScore += score
        case s if s == opponentPlayerSquare => opponentPositionalScore += score
        case _                              => // Empty square
    }
    myPositionalScore - opponentPositionalScore

/** Heurysytka oparta na "New Trends in Clobber Programming" H(g) = S(g)/S(p) jeśli S(g) > S(p) wpp -S(p)/S(g) S(g) dla
  * gracza: suma po jego bierkach (1 + sąsiednie po przekątnej swoje + sąsiednie (nie po przekątnej) przeciwne)
  */
object NewTrends extends Heuristic:
  override def evaluate(board: Board, forPlayer: Player): Double =
    val currentPlayerSquare = forPlayer.square
    val opponentPlayerSquare = forPlayer.other.square

    var s_g = 0.0 // Suma dla aktualnego gracza
    var s_p = 0.0 // Suma dla przeciwnika

    for (r <- 0 until board.numRows; c <- 0 until board.numCols) {
      board.grid(r)(c) match {
        case sq if sq == currentPlayerSquare =>
          s_g += 1.0 // Za samą bierkę
          s_g += countDiagonal(board, r, c, currentPlayerSquare)
          s_g += countAdjacent(board, r, c, opponentPlayerSquare)
        case sq if sq == opponentPlayerSquare =>
          s_p += 1.0 // Za samą bierkę
          s_p += countDiagonal(board, r, c, opponentPlayerSquare)
          s_p += countAdjacent(board, r, c, currentPlayerSquare)
        case _ => // Puste pole
      }
    }

    if (s_g == 0.0 && s_p == 0.0) {
      0.0
    } else if (s_p == 0.0) {
      if (s_g > 0.0) Double.PositiveInfinity else 0.0
    } else if (s_g == 0.0) {
      if (s_p > 0.0) Double.NegativeInfinity else 0.0
    } else if (s_g > s_p) {
      s_g / s_p
    } else {
      -s_p / s_g
    }

/** Heurystyka koncentracja na graniu na niekorzyść przeciwnika H(g) = 1/S(p) S(p) = (ilość pionków przeciwnika) + (suma
  * możliwych bić przeciwnika)
  */
object OpponentPenalty extends Heuristic:
  override def evaluate(board: Board, forPlayer: Player): Double =
    val currentPlayerSquare = forPlayer.square
    val opponentPlayerSquare = forPlayer.other.square

    var s_p = 0.0 // Suma dla przeciwnika

    for (r <- 0 until board.numRows; c <- 0 until board.numCols) {
      if (board.grid(r)(c) == opponentPlayerSquare) {
        s_p += 1.0 // Za bierkę przeciwnika
        s_p += countAdjacent(board, r, c, currentPlayerSquare) // Możliwe bicia tej bierki przeciwnika
      }
    }

    if (s_p == 0.0) {
      Double.PositiveInfinity // Przeciwnik nie ma bierek lub nie może wykonać żadnego bicia - bardzo dobrze
    } else {
      1.0 / s_p
    }

/** Heurystyka grupowanie pionków, kara za samotne pionki H(g) = S(g)/Samotne jeśli Samotne > 0 wpp S(g) + 1 S(g) = suma
  * po bierkach gracza (Sąsiednie_przeciwne * 2 + Sąsiednie_swoje)
  */
object GroupingLonely extends Heuristic:
  override def evaluate(board: Board, forPlayer: Player): Double =
    val currentPlayerSquare = forPlayer.square
    val opponentPlayerSquare = forPlayer.other.square

    var s_g_total = 0.0 // Cakowita suma dla gracza
    var lonely_pieces = 0 // Liczba samotnych bierek gracza

    for (r <- 0 until board.numRows; c <- 0 until board.numCols) {
      if (board.grid(r)(c) == currentPlayerSquare) {
        var cell_s_g = 0.0
        val adjacent_friendly = countAdjacent(board, r, c, currentPlayerSquare)
        val adjacent_opponent = countAdjacent(board, r, c, opponentPlayerSquare)

        cell_s_g += adjacent_opponent * 2.0
        cell_s_g += adjacent_friendly

        s_g_total += cell_s_g

        // Bierka jest samotna, jeśli jej wkład cell_s_g wynosi 0
        // (brak sąsiadów własnych i przeciwnika)
        if (cell_s_g == 0.0) {
          lonely_pieces += 1
        }
      }
    }

    if (lonely_pieces > 0) {
      if (s_g_total == 0.0) 0.0 // Uniknięcie NaN, 0/lonely = 0
      else s_g_total / lonely_pieces.toDouble
    } else { // Brak samotnych bierek - nagroda
      s_g_total + 1.0
    }

private object Helpers:
  def countAdjacent(board: Board, r: Int, c: Int, targetSquare: Square): Int = {
    var count = 0
    val dr = Array(-1, 1, 0, 0) // Zmiany wierszy: góra, dół
    val dc = Array(0, 0, -1, 1) // Zmiany kolumn: lewo, prawo
    for (i <- 0 until 4) {
      val nr = r + dr(i)
      val nc = c + dc(i)
      if (nr >= 0 && nr < board.numRows && nc >= 0 && nc < board.numCols) {
        if (board.grid(nr)(nc) == targetSquare) {
          count += 1
        }
      }
    }
    count
  }

  def countDiagonal(board: Board, r: Int, c: Int, targetSquare: Square): Int = {
    var count = 0
    val dr = Array(-1, -1, 1, 1) // Zmiany wierszy
    val dc = Array(-1, 1, -1, 1) // Zmiany kolumn
    for (i <- 0 until 4) {
      val nr = r + dr(i)
      val nc = c + dc(i)
      if (nr >= 0 && nr < board.numRows && nc >= 0 && nc < board.numCols) {
        if (board.grid(nr)(nc) == targetSquare) {
          count += 1
        }
      }
    }
    count
  }

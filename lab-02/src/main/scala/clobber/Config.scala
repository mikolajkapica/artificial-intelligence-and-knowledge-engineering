package clobber

import clobber.ai.Heuristic
import cats.effect.IO
import clobber.ai.MobilityHeuristic
import clobber.ai.PieceCountHeuristic
import clobber.ai.PositionalHeuristic
import scala.io.StdIn

enum Algorithm:
  case Minimax, AlphaBeta
  override def toString: String = this match {
    case Minimax   => "minimax"
    case AlphaBeta => "alphabeta"
  }

object Algorithm:
  def parse(name: String): Option[Algorithm] = name match {
    case "minimax"   => Some(Minimax)
    case "alphabeta" => Some(AlphaBeta)
    case _           => None
  }

case class PlayerAIConfig(heuristic: Heuristic, depth: Int, algorithm: Algorithm)

final case class AiConfigs(
    black: PlayerAIConfig,
    white: PlayerAIConfig
) {
  def get(player: Player): PlayerAIConfig = player match {
    case Player.Black => black
    case Player.White => white
  }
}

def getAIConfig(
    player: Player,
    defaultDepth: Int,
    defaultAlgorithm: Algorithm
): IO[PlayerAIConfig] = {
  val playerName = player.toString
  for {
    _ <- IO.println(
      s"""|
          |Configuring AI for $playerName:
          |Select heuristic for $playerName (mobility, piececount, positional):""".stripMargin
    )
    heuristic <- IO.readLine.map(_.trim.toLowerCase).map(Heuristic.parse).flatMap(IO.fromOption(_)(
      new IllegalArgumentException(s"Unknown heuristic. Valid options: mobility, piececount, positional.")
    ))

    _ <- IO.println(s"Enter search depth for $playerName (e.g., $defaultDepth):")
    depth <- IO.readLine.map(_.trim.toIntOption.getOrElse(defaultDepth))

    _ <- IO.println(s"Select search algorithm for $playerName (minimax or alphabeta, default: $defaultAlgorithm):")
    algorithm <- IO.readLine.map(_.trim.toLowerCase).flatMap {
      case "" => IO.pure(defaultAlgorithm)
      case algorithm =>
        IO.fromOption(Algorithm.parse(algorithm))(
          new IllegalArgumentException(s"Unknown algorithm: $algorithm. Valid options: minimax, alphabeta.")
        )
    }

    _ <- IO.println(
      s"$playerName AI configured: Heuristic=$heuristic, Depth=$depth, Algorithm=$algorithm"
    )
  } yield PlayerAIConfig(heuristic, depth, algorithm)
}

def readBoardConfig: IO[List[String]] = IO {
  LazyList
    .continually(StdIn.readLine())
    .takeWhile(line => line != null && line.nonEmpty)
    .toList
}

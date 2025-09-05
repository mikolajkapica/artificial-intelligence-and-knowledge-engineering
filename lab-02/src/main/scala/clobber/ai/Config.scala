package clobber.ai

import cats.effect.IO
import clobber.Player
import clobber.ai.Algorithm.{AlphaBeta, Minimax}

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
          |Select heuristic for $playerName (mobility, piececount, positional, newtrends, opponentpenalty, groupinglonely):""".stripMargin
    )
    heuristic <- IO.readLine.map(_.trim.toLowerCase).map(Heuristic.parse).flatMap(IO.fromOption(_)(
      new IllegalArgumentException(
        s"Unknown heuristic. Valid options: mobility, piececount, positional, newtrends, opponentpenalty, groupinglonely"
      )
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
  ()
//  LazyList
//    .continually(StdIn.readLine())
//    .takeWhile(line => line != null && line.nonEmpty)
//    .toList
}.flatMap { _ => IO.pure(`10x10`.split('\n').toList) }

val `10x10` =
  raw"""W B W B W B W B W B
       |B W B W B W B W B W
       |W B W B W B W B W B
       |B W B W B W B W B W
       |W B W B W B W B W B
       |B W B W B W B W B W
       |W B W B W B W B W B
       |B W B W B W B W B W
       |W B W B W B W B W B
       |B W B W B W B W B W""".stripMargin

val `5x5` =
  raw"""W B W B W
       |B W B W B
       |W B W B W
       |B W B W B
       |W B W B W""".stripMargin

val `5x8` =
  raw"""W B W B W B W B
       |B W B W B W B W
       |W B W B W B W B
       |B W B W B W B W
       |W B W B W B W B""".stripMargin

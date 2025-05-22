package clobber.model

enum Square:
  case B, W, Empty

object Square:
  def fromString(char: Char): Option[Square] = char match
    case 'B' => Some(Square.B)
    case 'W' => Some(Square.W)
    case '_' => Some(Square.Empty)
    case _   => None

  extension (s: Square)
    def stringRep: String = s match
      case Square.B     => "B"
      case Square.W     => "W"
      case Square.Empty => "_"

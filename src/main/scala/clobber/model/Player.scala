package clobber.model

enum Player:
  case Black, White
  def other: Player = this match
    case Black => White
    case White => Black
  def shortName: String = this match
    case Black => "B"
    case White => "W"

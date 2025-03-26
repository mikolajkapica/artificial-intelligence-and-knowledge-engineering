package algorithms.utils

import domain.Connection
import domain.Stop
import domain.Time

import scala.concurrent.duration.DurationInt

enum Optimization:
  case Time
  case Transfers
  case Combined

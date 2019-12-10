package aoc.trigo

import aoc.math._

object VectorUtils {
  def simplifyVector(v: Coord): Coord =
    v / Math.abs(gcd(v.x, v.y))

  def angle(v: Coord) =
    math.acos(v.x / math.sqrt(((v.x * v.x + v.y * v.y).toDouble))) / math.Pi *
      (v.y match {
        case a if a >= 0 => 1
        case a if a < 0  => -1
      })
}

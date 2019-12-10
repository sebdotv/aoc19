package d10

import aoc.geometry.Coord
import aoc.math._

object ReduceVector {
  def reduce(v: Coord): Coord =
    v / Math.abs(gcd(v.x, v.y))
}

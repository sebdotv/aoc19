package aoc.trigo

import cats.Eq
import cats.implicits._

case class Coord(x: Int, y: Int) {
  import Coord._
  def +(other: Coord): Coord = Coord(x + other.x, y + other.y)
  def -(other: Coord): Coord = Coord(x - other.x, y - other.y)
  def /(d: Int): Coord = {
    require(x % d === 0)
    require(y % d === 0)
    Coord(x / d, y / d)
  }
  def abs: Int                = dist(zero)
  def dist(other: Coord): Int = math.abs(x - other.x) + math.abs(y - other.y)
}
object Coord {
  val zero: Coord                 = Coord(0, 0)
  implicit val eqCoord: Eq[Coord] = Eq.fromUniversalEquals
}

package aoc.trigo

import cats.Eq
import cats.implicits._

import scala.annotation.tailrec

case class Coord(x: Int, y: Int) {
  import Coord._
  def unary_- : Coord        = Coord(-x, -y)
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

  @tailrec
  def computeRange(coords: List[Coord], minX: Int = 0, minY: Int = 0, maxX: Int = 0, maxY: Int = 0): (Int, Int, Int, Int) =
    coords match {
      case Nil    => (minX, minY, maxX, maxY)
      case h :: t => computeRange(t, minX = math.min(minX, h.x), minY = math.min(minY, h.y), maxX = math.max(maxX, h.x), maxY = math.max(maxY, h.y))
    }
}

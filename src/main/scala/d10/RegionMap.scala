package d10

import aoc.geometry.Coord
import cats.implicits._

case class RegionMap(w: Int, h: Int, asteroids: Set[Coord])
object RegionMap {
  def parse(lines: List[String]): RegionMap = {
    val w = lines.head.length
    val h = lines.size
    require(lines.forall(_.length === w))
    val asteroids =
      for {
        (line, y) <- lines.zipWithIndex
        (c, x)    <- line.zipWithIndex
        if (c match {
          case '#' => true
          case '.' => false
        })
      } yield Coord(x, y)
    RegionMap(w, h, asteroids.toSet)
  }
}

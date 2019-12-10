package d10

import aoc.trigo.Coord
import aoc.trigo.VectorUtils._
import cats.implicits._

object Part1 {
  def bestLocation(rm: RegionMap): (Coord, Int) = {
    val candidates = for (a <- rm.asteroids) yield a -> computeMinVectors(rm, a).size
    candidates.maxBy(_._2)
  }

  def computeMinVectors(rm: RegionMap, a: Coord): Set[Coord] =
    for (b <- rm.asteroids if b =!= a) yield simplifyVector(b - a)

  def detectionGrid(rm: RegionMap): String =
    (for (y <- 0 until rm.h) yield (for (x <- 0 until rm.w) yield {
      val coord = Coord(x, y)
      val aB    = rm.asteroids.apply(coord)
      if (aB) {
        val Array(c) = computeMinVectors(rm, coord).size.toHexString.toCharArray
        c
      } else '.'
    }).mkString).mkString("\n")

}

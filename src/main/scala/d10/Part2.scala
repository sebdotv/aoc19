package d10

import aoc.trigo.Coord
import aoc.trigo.VectorUtils._
import cats.implicits._

import scala.annotation.tailrec
import scala.util.Sorting

object Part2 {
  object ByLaserTargetOrdering extends Ordering[Coord] {
    def compare(a: Coord, b: Coord) = rotatedAngle(a) compare rotatedAngle(b)
  }

  case class ByDistOrdering(origin: Coord) extends Ordering[Coord] {
    def compare(a: Coord, b: Coord) = a.dist(origin) compare b.dist(origin)
  }

  def rotatedAngle(v: Coord) = (angle(v) - 0.5) match {
    case a if a < -1 => a + 2
    case other       => other
  }

  def sorted(ordering: Ordering[Coord])(coords: TraversableOnce[Coord]): List[Coord] = {
    val arr = coords.toArray
    Sorting.quickSort(arr)(ordering)
    arr.toList
  }

  def vaporizationOrder(rm: RegionMap, station: Coord): List[Coord] = {
    val asteroidsByMinVector = rm.asteroids
      .filterNot(_ === station)
      .groupBy(a => simplifyVector(a - station))
      .mapValues(sorted(ByDistOrdering(station)))
    val minVectors       = asteroidsByMinVector.keys
    val sortedMinVectors = sorted(ByLaserTargetOrdering)(minVectors)

    @tailrec
    def vaporizeAsteroids(dirIndex: Int = 0, vaporized: List[Coord] = Nil): List[Coord] =
      if (vaporized.size === rm.asteroids.size - 1) vaporized
      else {
        val dir          = sortedMinVectors(dirIndex)
        val targetO      = asteroidsByMinVector(dir).view.filterNot(vaporized.contains _).headOption
        val nextDirIndex = (dirIndex + 1) % sortedMinVectors.size
        targetO match {
          case None =>
            vaporizeAsteroids(nextDirIndex, vaporized)
          case Some(target) =>
            vaporizeAsteroids(nextDirIndex, target :: vaporized)
        }
      }

    vaporizeAsteroids().reverse
  }

  // supports max 9 coords
  def laserGrid(rm: RegionMap, station: Coord, vaporizationOrder: List[Coord]): String = {
    val vaporizedReverseMap = vaporizationOrder.zipWithIndex.map { case (a, i) => (a, i + 1) }.toMap
    (for (y <- 0 until rm.h) yield (for (x <- 0 until rm.w) yield {
      val coord = Coord(x, y)
      val aB    = rm.asteroids.apply(coord)
      if (aB) {
        if (coord === station) 'X'
        else {
          vaporizedReverseMap
            .get(coord)
            .map(n => {
              val Array(c) = n.toString.toCharArray
              c
            })
            .getOrElse('#')
        }
      } else '.'
    }).mkString).mkString("\n")
  }
}

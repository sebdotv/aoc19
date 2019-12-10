package d03

import aoc.trigo.Coord
import d03.Part1._
import cats.implicits._

object Part2 {
  def result(s1: String, s2: String): Int = {
    def trace(s: String) =
      walk(parse(s), Coord.zero).reverse.zipWithIndex
        .map { case (c, i) => (c, i + 1) }

    val t1     = trace(s1)
    val t2     = trace(s2)
    val traces = List(t1, t2)

    val intersects = t1.map(_._1).toSet.intersect(t2.map(_._1).toSet)
    intersects
      .map(i => (for (t <- traces) yield t.find(_._1 === i).get).map(_._2).sum)
      .min
  }
}

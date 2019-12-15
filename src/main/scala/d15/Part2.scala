package d15

import aoc.trigo.Coord
import cats.implicits._
import d15.Cell._

import scala.annotation.tailrec

case class Sim(map: AreaMap, frontier: List[Coord], minutes: Int = 0) {
  def step: Sim = {
    val growth =
      for {
        p        <- frontier
        neighbor <- map.around(p)
        m        = neighbor._1
        cellO    = neighbor._2
        if cellO === Some(Empty)
      } yield p -> m
    Sim(
      map = map.add(for ((p, m) <- growth) yield (p + m.v) -> Oxygen),
      frontier = for ((p, m) <- growth) yield p + m.v,
      minutes = minutes + 1
    )
  }
  @tailrec
  final def run: Sim =
    if (frontier.isEmpty) this
    else step.run
}

object Part2 {
  def result(map: AreaMap): Int = {
    val sim = Sim(map, map.explored.collect { case (k, v) if v === Oxygen => k }.toList)
    sim.run.minutes - 1 // simulation stops when nothing was done, so 1 minute after the area was filled
  }
}

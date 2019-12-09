package d05

import aoc.intcode.Program

object Part1 {
  def result(p: Program): List[Long] =
    p.runOn(List(1))
}

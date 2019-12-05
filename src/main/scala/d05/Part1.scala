package d05

import aoc.intcode.Program

object Part1 {
  def result(p: Program): List[Int] =
    p.runOn(List(1))
}

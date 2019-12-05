package d05

import aoc.intcode.Program

object Part2 {
  def result(p: Program): Int =
    p.runFn(5)
}

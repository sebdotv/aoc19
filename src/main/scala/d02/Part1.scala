package d02

import aoc.intcode.Program

object Part1 {
  def patch(p: Program, noun: Int, verb: Int): Program =
    p.write(1, noun).write(2, verb)
}

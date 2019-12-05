package d05

import aoc.intcode.Program

object Part1 {
  def result(input: String): List[Int] =
    Program.parse(input).runOn(List(1))
}

package d02

import aoc.intcode.Program
import cats.implicits._

object Part2 {
  def result(p: Program): Int = {
    val resultS =
      for {
        noun <- (0 to 99).toStream
        verb <- (0 to 99).toStream
        if Part1.patch(p, noun = noun, verb = verb).run.read(0) === 19690720
      } yield 100 * noun + verb
    resultS.head
  }
}

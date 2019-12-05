package d02

import aoc.intcode.Program
import cats.implicits._

object Part2 {
  def result(p: Program): Int = {
    val resultS =
      for {
        noun <- (0 to 99).toStream
        verb <- (0 to 99).toStream
        if p.patch(noun = noun, verb = verb).run.getMemory(0) === 19690720
      } yield 100 * noun + verb
    resultS.head
  }
}

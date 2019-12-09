package d02

import aoc.intcode.Program
import cats.implicits._

object Part2 {
  def result(p: Program): Long = {
    val resultS =
      for {
        noun <- (0L to 99L).toStream
        verb <- (0L to 99L).toStream
        if Part1.patch(p, noun = noun, verb = verb).run.read(0) === 19690720
      } yield 100L * noun + verb
    resultS.head
  }
}

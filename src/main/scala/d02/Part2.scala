package d02

import d02.Part1.Program
import cats.implicits._

object Part2 {
  def result(p: Program): Int = {
    val results =
      for {
        noun    <- 0 to 99
        verb    <- 0 to 99
        program = p.patch(noun = noun, verb = verb)
        if program.run.getMemory(0) === 19690720
      } yield 100 * noun + verb
    results match {
      case Seq(a) => a
    }
  }
}

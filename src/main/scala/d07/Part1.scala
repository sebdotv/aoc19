package d07

import aoc.intcode.Program

object Part1 {
  def result(input: String): (List[Int], Int) =
    (for (phases <- (0 to 4).toList.permutations) yield phases -> result(input, phases)).maxBy(_._2)

  def result(input: String, phases: List[Int]): Int = {
    val p = Program.parse(input)
    phases.reverse.foldRight(0) {
      case (phase, input) =>
        val List(output) = p.runOn(List(phase, input))
        output
    }
  }
}

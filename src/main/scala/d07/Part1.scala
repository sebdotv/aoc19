package d07

import aoc.intcode.Program

object Part1 {
  def result(input: String): (List[Long], Long) =
    (for (phases <- (0L to 4L).toList.permutations) yield phases -> result(input, phases)).maxBy(_._2)

  def result(input: String, phases: List[Long]): Long = {
    val p = Program.parse(input)
    phases.foldLeft(0L) {
      case (input, phase) =>
        val List(output) = p.runOn(List(phase, input))
        output
    }
  }
}

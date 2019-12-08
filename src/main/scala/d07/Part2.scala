package d07

import aoc.intcode.Program
import cats.Show
import cats.data.State
import cats.implicits._
import aoc.implicits._
import aoc.intcode.Program.State.Halted

import scala.annotation.tailrec

object Part2 {
  def result(input: String): (List[Int], Int) =
    (for (phases <- (5 to 9).toList.permutations) yield phases -> result(input, phases)).maxBy(_._2)

  case class Amps(programs: Array[Program])
  object Amps {
    def runAmp(i: Int, inputValue: Int): State[Amps, Int] =
      State(amps => {
        val (Some(outputValue), updatedP) = amps.programs(i - 1).feed(inputValue).run.extractOutput
        val updatedPrograms               = amps.programs.clone
        updatedPrograms(i - 1) = updatedP
        (amps.copy(programs = updatedPrograms), outputValue)
      })
    implicit val showProgram: Show[Program] = Show.show(p => show"[${p.state} ${p.input.size} ${p.output.size}]")
    implicit val showAmps: Show[Amps]       = Show.show(amps => amps.programs.show)
  }

  def result(input: String, phases: List[Int]): Int = {
    val p        = Program.parse(input)
    val programs = phases.map(phase => p.feed(phase))
    val amps     = Amps(programs.toArray)

    def loop(inputValue: Int) =
      for {
        o <- Amps.runAmp(1, inputValue)
        o <- Amps.runAmp(2, o)
        o <- Amps.runAmp(3, o)
        o <- Amps.runAmp(4, o)
        o <- Amps.runAmp(5, o)
      } yield o
    @tailrec
    def rec(x: (Amps, Int)): (Amps, Int) = {
      if (x._1.programs.forall(_.state === Halted)) x
      else rec(loop(x._2).run(x._1).value)
    }

    val finalAmps = rec((amps, 0))
    finalAmps._2
  }
}

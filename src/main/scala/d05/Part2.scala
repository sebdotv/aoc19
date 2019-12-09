package d05

import aoc._
import aoc.intcode.Program
import cats.effect.{ExitCode, IO, IOApp}

object Part2 {
  def result(p: Program): Long =
    p.runFn(5)
}

object TestApp extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    for {
      line   <- loadLine("input/05.txt")
      input  = Program.parse(line).copy(debug = true)
      result = Part2.result(input)
    } yield ExitCode.Success
}

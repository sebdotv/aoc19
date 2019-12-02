package aoc

import cats.effect.{ExitCode, IO, IOApp}

abstract class AocApp(filename: String) extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    for {
      lines  <- load(filename)
      result = program(lines)
      _      <- IO(println(result))
    } yield ExitCode.Success

  def program(lines: List[String]): Any
}

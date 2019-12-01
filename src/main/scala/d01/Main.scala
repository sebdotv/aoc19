package d01

import java.nio.file.Paths

import cats.effect._
import cats.implicits._
import fs2._
import mouse.all._

object Main extends IOApp {
  // to find the fuel required for a module, take its mass, divide by three, round down, and subtract 2
  def massToFuel(m: Long) = m / 3 - 2

  val converter: Stream[IO, Unit] = Stream.resource(Blocker[IO]).flatMap {
    blocker =>
      io.file
        .readAll[IO](Paths.get("input/01.txt"), blocker, 4096)
        .through(text.utf8Decode)
        .through(text.lines)
        .filter(!_.isEmpty)
        .evalMap(_.parseLong.liftTo[IO])
        .map(massToFuel)
        .fold(0L)(_ + _)
        .through(io.stdoutLines(blocker))
  }

  def run(args: List[String]): IO[ExitCode] =
    converter.compile.drain.as(ExitCode.Success)
}

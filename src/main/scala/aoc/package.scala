import java.nio.file.{Files, Paths}

import cats.effect.IO

import scala.collection.JavaConverters._

package object aoc extends ShowInstances {
  def load(filename: String): IO[List[String]] =
    IO(Files.readAllLines(Paths.get(filename)))
      .map(_.asScala.toList)
}

package d01

import java.nio.file.{Files, Paths}

import scala.annotation.tailrec
import scala.collection.JavaConverters._

object Part2 extends App {
  val lines = Files.readAllLines(Paths.get("input/01.txt")).asScala

  def totalFuel(m: Long) = {
    @tailrec
    def it(m: Long, acc: Long): Long =
      m match {
        case 0L => acc
        case _ =>
          val fuel = math.max(0L, m / 3 - 2)
          it(fuel, acc + fuel)
      }
    it(m, 0)
  }

  println(lines.map(_.toLong).map(totalFuel).sum)
}

package d01

import java.nio.file.{Files, Paths}
import scala.collection.JavaConverters._

object Part2 extends App {
  val lines = Files.readAllLines(Paths.get("input/01.txt")).asScala

  def totalFuel(m: Long): Long =
    m match {
      case 0L => 0L
      case _ =>
        val result = math.max(0L, m / 3 - 2)
        result + totalFuel(result)
    }

  println(lines.map(_.toLong).map(totalFuel).sum)
}

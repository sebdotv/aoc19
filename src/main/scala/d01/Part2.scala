package d01

import scala.annotation.tailrec

object Part2 {
  def totalFuel(m: Long) = {
    @tailrec
    def it(m: Long, acc: Long): Long =
      m match {
        case 0L => acc
        case _ =>
          val fuel = math.max(0L, Part1.massToFuel(m))
          it(fuel, acc + fuel)
      }
    it(m, 0)
  }

  def result(lines: List[String]) =
    lines.map(_.toLong).map(totalFuel).sum
}

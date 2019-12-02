package d01

object Part1 {
  // to find the fuel required for a module, take its mass, divide by three, round down, and subtract 2
  def massToFuel(m: Long): Long = m / 3 - 2

  def result(lines: List[String]): Long =
    lines.map(_.toLong).map(massToFuel).sum
}

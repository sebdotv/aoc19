package d11

import aoc.trigo.Coord

object Part2 {
  def render(paintedPanels: Map[Coord, Long]): String = {
    val (minX, minY, maxX, maxY) = Coord.computeRange(paintedPanels.keys.toList)
    (for (y <- maxY to minY by -1) yield (for (x <- minX to maxX) yield paintedPanels.getOrElse(Coord(x, y), 0) match {
      case 0 => ' '
      case 1 => '#'
    }).mkString).mkString("\n")
  }
}

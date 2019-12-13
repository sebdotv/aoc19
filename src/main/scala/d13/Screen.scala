package d13

import aoc.trigo.Coord
import d13.TileType._

case class Screen(painted: Map[Coord, TileType] = Map.empty, segmentDisplay: Option[Int] = None) {
  def dump: String =
    (s"score: ${segmentDisplay.getOrElse("?")}" :: renderTiles).mkString("\n")

  def renderTiles: List[String] = {
    val (minX, minY, maxX, maxY) = Coord.computeRange(painted.keys.toList)
    (for (y <- minY to maxY) yield (for (x <- minX to maxX) yield painted.getOrElse(Coord(x, y), Empty) match {
      case Empty  => ' '
      case Wall   => '#'
      case Block  => '@'
      case Paddle => '='
      case Ball   => 'o'
    }).mkString).toList
  }

  def set(coord: Coord, value: Int): Screen =
    coord match {
      case Coord(-1, 0) => copy(segmentDisplay = Some(value))
      case _ =>
        val tileType = TileType.fromId(value)
        copy(painted = tileType match {
          case TileType.Empty => painted - coord
          case _              => painted + (coord -> tileType)
        })
    }
}

package d11

import aoc.intcode.Program
import aoc.intcode.Program.ProgramState._
import aoc.intcode.Program._
import aoc.trigo.Coord
import d11.Part1.Direction._

import scala.annotation.tailrec

object Part1 {
  sealed case class Direction(v: Coord)
  object Direction {
    object Up    extends Direction(Coord(0, 1))
    object Down  extends Direction(Coord(0, -1))
    object Left  extends Direction(Coord(-1, 0))
    object Right extends Direction(Coord(1, 0))
    val clockwise = List(Up, Right, Down, Left)
  }
  case class Region(p: Program, paintedPanels: Map[Coord, Long] = Map.empty, robot: Coord = Coord.zero, dir: Direction = Up) {
    def step: Region = {
      val state =
        for {
          _           <- feedAndRunS(paintedPanels.getOrElse(robot, 0L))
          colorO      <- extractOutputS
          Some(color) = colorO
          turnO       <- extractOutputS
          Some(turn)  = turnO
          updatedDir = clockwise((clockwise.indexOf(dir) + (turn match {
            case 0 => -1 // left
            case 1 => 1  // right
          }) + clockwise.size) % clockwise.size)
        } yield (paintedPanels + (robot -> color), robot + updatedDir.v, updatedDir)
      state.run(p).value match {
        case (updatedP, (updatedPaintedPanels, updatedRobot, updatedDir)) =>
          copy(updatedP, updatedPaintedPanels, updatedRobot, updatedDir)
      }
    }
    @tailrec
    final def run: Region =
      p.state match {
        case Running | Blocked => step.run
        case Halted            => this
      }
  }

  object Region {}

}

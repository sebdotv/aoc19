package d13

import aoc.intcode.Program.ProgramState._
import cats.implicits._
import d13.TileType._

import scala.annotation.tailrec

object Part2 {
  @tailrec
  def run(a: ArcadeCabinet): ArcadeCabinet =
    a.p.state match {
      case Running => run(a.step)
      case Blocked =>
        val dir =
          a.screen.painted.find(_._2 === Ball).get._1 -
            a.screen.painted.find(_._2 === Paddle).get._1
        val joystick = math.signum(dir.x)
        run(a.copy(p = a.p.feed(joystick.toLong)))
      case Halted => a
    }
}

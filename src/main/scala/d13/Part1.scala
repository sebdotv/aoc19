package d13

import aoc.intcode.Program
import aoc.intcode.Program.ProgramState.Halted
import aoc.intcode.Program._
import aoc.trigo.Coord
import cats.data.State
import cats.implicits._

import scala.annotation.tailrec

sealed abstract case class TileType(id: Int)
object TileType {
  object Empty  extends TileType(0)
  object Wall   extends TileType(1)
  object Block  extends TileType(2)
  object Paddle extends TileType(3)
  object Ball   extends TileType(4)
  val values          = List(Empty, Wall, Block, Paddle, Ball)
  def fromId(id: Int) = values.find(_.id === id).get
}

case class Screen(painted: Map[Coord, TileType] = Map.empty, segmentDisplay: Option[Int] = None) {
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

case class ArcadeCabinet(p: Program, screen: Screen = Screen()) {
  def step: ArcadeCabinet = {
    val state =
      for {
        x      <- runToOutputS
        y      <- runToOutputS
        tileId <- runToOutputS
      } yield (x, y, tileId) match {
        case (Some(x), Some(y), Some(value)) =>
          screen.set(Coord(x.toInt, y.toInt), value.toInt)
        case (None, None, None) =>
          screen
        case _ => throw new IllegalStateException
      }
    val (up, us) = state.run(p).value
    ArcadeCabinet(up, us)
  }
}
object ArcadeCabinet {
  val stepS: State[ArcadeCabinet, ProgramState] = State { a =>
    val ua = a.step
    (ua, ua.p.state)
  }
}

object Part1 {
  @tailrec
  def run(a: ArcadeCabinet): ArcadeCabinet =
    if (a.p.state === Halted) a
    else run(a.step)
}

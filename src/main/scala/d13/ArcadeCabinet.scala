package d13

import aoc.intcode.Program
import aoc.intcode.Program._
import aoc.trigo.Coord
import cats.data.State

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

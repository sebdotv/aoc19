package d13

import aoc.intcode.Program.ProgramState._
import cats.implicits._

import scala.annotation.tailrec

object Part1 {
  @tailrec
  def run(a: ArcadeCabinet): ArcadeCabinet =
    if (a.p.state === Running) run(a.step)
    else a

  def countBlocks(a: ArcadeCabinet) =
    run(a).screen.painted.values.count(_ === TileType.Block)
}

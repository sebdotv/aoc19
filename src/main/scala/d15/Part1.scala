package d15

import aoc.intcode.Program
import aoc.intcode.Program._
import aoc.trigo.Coord
import cats.Eq
import cats.implicits._
import d15.Cell.Empty
import d15.ControllerState.{Done, OxygenFound, Running}

import scala.annotation.tailrec

sealed trait Cell
object Cell {
  object Empty  extends Cell
  object Wall   extends Cell
  object Oxygen extends Cell
}

sealed case class Movement(command: Int, v: Coord) {
  import Movement._
  def reverse: Movement = clockwise.find(_.v === -v).get
}
object Movement {
  object North extends Movement(1, Coord(0, 1)) {
    override def toString: String = "North"
  }
  object South extends Movement(2, Coord(0, -1)) {
    override def toString: String = "South"
  }
  object West extends Movement(3, Coord(-1, 0)) {
    override def toString: String = "West"
  }
  object East extends Movement(4, Coord(1, 0)) {
    override def toString: String = "East"
  }
  val clockwise = List(North, East, South, West)
}

case class AreaMap(explored: Map[Coord, Cell] = Map.empty) {
  import Cell._
  def add(pos: Coord, cell: Cell): AreaMap        = copy(explored = explored + (pos -> cell))
  def add(mappings: List[(Coord, Cell)]): AreaMap = copy(explored = explored ++ mappings)
  def around(pos: Coord): List[(Movement, Option[Cell])] =
    for (m <- Movement.clockwise) yield m -> explored.get(pos + m.v)
  def dump(droidO: Option[Coord] = None): String = {
    val chars = (explored.mapValues {
      case Empty  => '.'
      case Wall   => '#'
      case Oxygen => 'X'
    } ++ droidO.map(d => Map(d -> 'D')).getOrElse(Map.empty)).withDefaultValue(' ')
    val (minX, minY, maxX, maxY) = Coord.computeRange(chars.keys.toList)
    (for (y <- maxY to minY by -1) yield (for (x <- minX to maxX) yield chars(Coord(x, y))).mkString).mkString("\n")
  }
}

sealed trait ControllerState
object ControllerState {
  object Running     extends ControllerState
  object OxygenFound extends ControllerState
  object Done        extends ControllerState
  implicit val eqControllerState: Eq[ControllerState] = Eq.fromUniversalEquals
}

case class Controller(p: Program, map: AreaMap, pos: Coord, path: List[Movement] = Nil, state: ControllerState = Running) {
  import Cell._

  def dump   = map.dump(pos.some)
  def around = map.around(pos)

  def backtrack: Controller =
    path match {
      case x :: xs =>
        move(x.reverse)
          .copy(path = xs) // override path, ignoring backtrack
      case Nil =>
        copy(state = Done)
    }

  def move(m: Movement): Controller = {
    val state =
      for {
        _       <- feedS(m.command.toLong)
        outputO <- runToOutputS
      } yield outputO
    val (pU, outputO) = state.run(p).value
    outputO match {
      case Some(output) =>
        val (cell, moved, stateU) =
          output match {
            case 0 => (Wall, false, Running)
            case 1 => (Empty, true, Running)
            case 2 => (Oxygen, true, OxygenFound)
          }
        val targetPos = pos + m.v
        copy(
          map = map.add(targetPos, cell),
          p = pU,
          pos = if (moved) targetPos else pos,
          path = if (moved) m :: path else path,
          state = stateU
        )
      case None =>
        copy(p = pU)
    }
  }
}
object Controller {
  def apply(p: Program): Controller = {
    val droid = Coord.zero
    Controller(p, AreaMap(Map(droid -> Empty)), droid)
  }
}

object Part1 {
  @tailrec
  def runUntil(c: Controller, state: ControllerState): Controller =
    if (c.state === state) c
    else {
      val unexplored = c.around.collect { case (m, None) => m }.headOption
      runUntil(unexplored.map(c.move).getOrElse(c.backtrack), state)
    }
}

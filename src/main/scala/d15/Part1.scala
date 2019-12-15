package d15

import aoc.intcode.Program
import aoc.intcode.Program._
import aoc.trigo.Coord
import d15.Cell.Empty
import cats.implicits._

import scala.annotation.tailrec
import scala.collection.immutable.Queue

sealed trait Cell
object Cell {
  object Empty        extends Cell
  object Wall         extends Cell
  object OxygenSystem extends Cell
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
  def add(pos: Coord, cell: Cell): AreaMap = copy(explored = explored + (pos -> cell))
  def around(pos: Coord): List[(Movement, Option[Cell])] =
    for (m <- Movement.clockwise) yield m -> explored.get(pos + m.v)
  def dump(droid: Coord): String = {
    val chars = (explored.mapValues {
      case Empty        => '.'
      case Wall         => '#'
      case OxygenSystem => 'X'
    } + (Coord.zero -> 'O') + (droid -> 'D')).withDefaultValue(' ')
    val (minX, minY, maxX, maxY) = Coord.computeRange(chars.keys.toList)
    (for (y <- maxY to minY by -1) yield (for (x <- minX to maxX) yield chars(Coord(x, y))).mkString).mkString("\n")
  }
}

case class Controller(p: Program, map: AreaMap, pos: Coord, path: List[Movement] = Nil, done: Boolean = false) {
  import Cell._

  def dump = map.dump(pos)

  def around = map.around(pos)

  def backtrack: Controller =
    path match {
      case x :: xs =>
        val m = x.reverse
//        println(s"backtrack: $x -> $m")
        step(m)
          .copy(path = xs) // override path, ignoring backtrack
      case Nil =>
        copy(done = true)
    }

  def step(m: Movement): Controller = {
    val state =
      for {
        _       <- feedS(m.command.toLong)
        outputO <- runToOutputS
      } yield outputO
    val (pU, outputO) = state.run(p).value
    outputO match {
      case Some(output) =>
        val (cell, moved) =
          output match {
            case 0 => (Wall, false)
            case 1 => (Empty, true)
            case 2 =>
              println(path.size + 1)
              (OxygenSystem, true)
          }
        val targetPos = pos + m.v
        copy(map = map.add(targetPos, cell), p = pU, pos = if (moved) targetPos else pos, path = if (moved) m :: path else path)
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
  import Cell._
  def x(p: Program) = {
    val c = Controller(p)
    loop(c)

    @tailrec
    def loop(c: Controller): Controller = {
//      println
      if (c.done) {
        println(c.dump)
        c
      } else {
//        println(c.dump)
//        //      println(c.map.explored.size)
        val unexplored = c.around.collect { case (m, None) => m }.headOption
//        println(unexplored)
        loop(unexplored.map(c.step).getOrElse(c.backtrack))
      }
    }
  }
}

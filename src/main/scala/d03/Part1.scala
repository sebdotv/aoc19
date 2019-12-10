package d03

import aoc.trigo.Coord

import scala.annotation.tailrec

object Part1 {
  def result(s1: String, s2: String): Int = {
    def trace(s: String) = walk(parse(s), Coord.zero).toSet
    val t1               = trace(s1)
    val t2               = trace(s2)
    val intersect        = t1.intersect(t2)
    intersect.map(_.abs).min
  }

  def walk(p: Path, start: Coord): List[Coord] = {
    @tailrec
    def it(p: Path, pos: Coord, acc: List[Coord]): List[Coord] = {
      p match {
        case Nil => acc
        case m :: tail =>
          val trace = m(pos)
          it(tail, trace.head, trace ::: acc)
      }
    }
    it(p, start, Nil)
  }

  def parse(path: String): List[Move] = path.split(",").toList.map(toMove)
  def toMove(s: String): Move = {
    val (dS, n) = s.splitAt(1)
    val Seq(d)  = dS.toSeq
    Move(d, n.toInt)
  }
  case class Move(d: Char, n: Int) {
    def apply(p: Coord): List[Coord] = {
      val dir = d match {
        case 'U' => Coord(0, 1)
        case 'D' => Coord(0, -1)
        case 'L' => Coord(-1, 0)
        case 'R' => Coord(1, 0)
      }
      @tailrec
      def it(n: Int, p: Coord, acc: List[Coord]): List[Coord] =
        if (n == 0) acc
        else {
          val upos = p + dir
          it(n - 1, upos, upos :: acc)
        }
      it(n, p, Nil)
    }
  }
  type Path = List[Move]
}

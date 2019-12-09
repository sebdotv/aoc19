package aoc.intcode

sealed trait Param

object Param {
  case class ImmediateParam(value: Int) extends Param {
    override def toString = s"$value"
  }

  case class PositionParam(position: Int) extends Param {
    override def toString = s"[$position]"
  }

  case class RelativeBaseParam(offset: Int) extends Param {
    override def toString = s"[RB± $offset]"
  }
}

package aoc.intcode

sealed trait Param

object Param {
  case class ImmediateParam(value: Long) extends Param {
    override def toString = s"$value"
  }

  case class PositionParam(position: Long) extends Param {
    override def toString = s"[$position]"
  }

  case class RelativeBaseParam(offset: Long) extends Param {
    override def toString = s"[RB± $offset]"
  }
}

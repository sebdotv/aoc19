package aoc.intcode
import cats.implicits._

sealed trait Param

object Param {
  case class ImmediateParam(value: Long) extends Param {
    override def toString = s"$value"
  }

  sealed trait PositionParam extends Param
  case class AbsolutePositionParam(position: Long) extends PositionParam {
    override def toString = s"[$position]"
  }
  case class RelativeBaseParam(offset: Long) extends PositionParam {
    override def toString =
      s"[RB${offset match {
        case x if x < 0   => x.toString
        case x if x === 0 => ""
        case x if x > 0   => s"+$x"
      }}]"
  }
}

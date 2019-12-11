package aoc.intcode

import aoc.intcode.Param._
import aoc.intcode.Program.ProgramState._
import cats.implicits._

sealed trait Instruction {
  def apply(p: Program): Program
}

object Instruction {
  case class ADD(p1: Param, p2: Param, dest: PositionParam) extends Instruction {
    override def apply(p: Program): Program = p.w(dest, p.r(p1) + p.r(p2)).move(4)
    override def toString                   = s"$dest = $p1 + $p2"
  }
  case class MUL(p1: Param, p2: Param, dest: PositionParam) extends Instruction {
    override def apply(p: Program): Program = p.w(dest, p.r(p1) * p.r(p2)).move(4)
    override def toString                   = s"$dest = $p1 * $p2"
  }
  case class IN(dest: PositionParam) extends Instruction {
    override def apply(p: Program): Program = p.in(dest) match {
      case p if p.state === Running => p.move(2)
      case p if p.state === Blocked => p
    }
    override def toString = s"$dest <- IN"
  }
  case class OUT(p1: Param) extends Instruction {
    override def apply(p: Program): Program = p.out(p.r(p1)).move(2)
    override def toString                   = s"$OUT <- $p1"
  }
  case class JNZ(t: Param, target: Param) extends Instruction {
    override def apply(p: Program): Program = p.r(t) match {
      case 0 => p.move(3) // nop
      case _ => p.jump(p.r(target))
    }
    override def toString = s"if ($t != 0) goto $target"
  }
  case class JZ(t: Param, target: Param) extends Instruction {
    override def apply(p: Program): Program = p.r(t) match {
      case 0 => p.jump(p.r(target))
      case _ => p.move(3) // nop
    }
    override def toString = s"if ($t == 0) goto $target"
  }
  case class LT(p1: Param, p2: Param, dest: PositionParam) extends Instruction {
    override def apply(p: Program): Program = p.w(dest, if (p.r(p1) < p.r(p2)) 1 else 0).move(4)
    override def toString                   = s"$dest = if ($p1 < $p2) 1 else 0"
  }
  case class EQ(p1: Param, p2: Param, dest: PositionParam) extends Instruction {
    override def apply(p: Program): Program = p.w(dest, if (p.r(p1) === p.r(p2)) 1 else 0).move(4)
    override def toString                   = s"$dest = if ($p1 == $p2) 1 else 0"
  }
  case class RBO(delta: Param) extends Instruction {
    override def apply(p: Program): Program = p.adjustRelativeBase(p.r(delta)).move(2)
    override def toString                   = s"RB += $delta"
  }
  case object HLT extends Instruction {
    override def apply(p: Program): Program = p.halt
    override def toString                   = "HLT"
  }
}

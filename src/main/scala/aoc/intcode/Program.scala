package aoc.intcode

import aoc.implicits._
import aoc.intcode.Instruction._
import aoc.intcode.Param._
import aoc.intcode.Program.ProgramState._
import cats.data.State
import cats.implicits._
import cats.{Eq, Show, derived}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

case class Program(
    sparseMemory: Map[Long, Long],
    ip: Long = 0,
    relativeBase: Long = 0,
    state: Program.ProgramState = Running,
    input: Queue[Long] = Queue.empty,
    output: Queue[Long] = Queue.empty,
    debug: Boolean = false
) {
  def adjustRelativeBase(delta: Long): Program =
    copy(relativeBase = relativeBase + delta)

  def r(param: Param): Long =
    param match {
      case p: PositionParam      => read(resolvePosition(p))
      case ImmediateParam(value) => value
    }
  private def resolvePosition(param: PositionParam): Long =
    param match {
      case AbsolutePositionParam(position) => position
      case RelativeBaseParam(delta)        => relativeBase + delta
    }

  def w(dest: PositionParam, value: Long): Program =
    write(resolvePosition(dest), value)
  def in(dest: PositionParam): Program =
    input.dequeueOption match {
      case Some((h, t)) =>
        if (debug) println(s"IN> $h")
        w(dest, h).copy(input = t)
      case None =>
        if (debug) println("IN> no input, blocked")
        setState(Blocked)
    }

  def out(value: Long): Program = {
    if (debug) println(s"OUT> $value")
    copy(output = output.enqueue(value))
  }

  def move(n: Long): Program = {
    require(n > 0)
    copy(ip = ip + n)
  }
  def jump(position: Long): Program = {
    require(position >= 0)
    copy(ip = position)
  }
  def halt: Program =
    setState(Halted)

  private def setState(s: Program.ProgramState): Program = {
    s match {
      case Halted =>
        require(state === Running)
      case Blocked =>
        require(state === Running)
      case Running =>
        require(state === Blocked)
    }
    copy(state = s)
  }

  def memory: List[Long] =
    (for (i <- 0L to sparseMemory.keys.max) yield read(i)).toList

  def read(position: Long): Long =
    sparseMemory.withDefaultValue(0L)(position)

  def write(position: Long, value: Long): Program = {
    copy(sparseMemory = sparseMemory + (position -> value))
  }

  /** @param param 1-based */
  private def param(param: Int, ic: InstructionCode): Param = {
    val value = read(ip + param)
    ic.parameterModes(param - 1) match {
      case 0 => // position mode
        AbsolutePositionParam(value)
      case 1 => // immediate mode
        ImmediateParam(value)
      case 2 => // relative mode
        RelativeBaseParam(value)
    }
  }

  def step: Program = {
    assert(state === Running)
    val ic = InstructionCode.parse(read(ip).toInt)
    val instructionO =
      ic.opcode match {
        case 1 =>
          param(3, ic).some.collect {
            case dest: PositionParam => ADD(param(1, ic), param(2, ic), dest)
          }
        case 2 =>
          param(3, ic).some.collect {
            case dest: PositionParam => MUL(param(1, ic), param(2, ic), dest)
          }
        case 3 =>
          param(1, ic).some.collect {
            case dest: PositionParam => IN(dest)
          }
        case 4 =>
          OUT(param(1, ic)).some
        case 5 =>
          JNZ(param(1, ic), param(2, ic)).some
        case 6 =>
          JZ(param(1, ic), param(2, ic)).some
        case 7 =>
          param(3, ic).some.collect {
            case dest: PositionParam => LT(param(1, ic), param(2, ic), dest)
          }
        case 8 =>
          param(3, ic).some.collect {
            case dest: PositionParam => EQ(param(1, ic), param(2, ic), dest)
          }
        case 9 =>
          RBO(param(1, ic)).some
        case 99 =>
          HLT.some
      }
    val instruction = instructionO.getOrElse(throw new RuntimeException(show"Unsupported: $ic"))
    if (debug) println(f"$ip%5s $instruction")
    instruction.apply(this)
  }

  @tailrec
  final def run: Program =
    state match {
      case Halted  => this
      case Blocked => this
      case Running => step.run
    }

  @tailrec
  final def runToOutput: (Program, Option[Long]) =
    extractOutput match {
      case (p, Some(o)) => (p, Some(o))
      case (p, None) =>
        state match {
          case Halted  => (p, None)
          case Blocked => (p, None)
          case Running => p.step.runToOutput
        }
    }

  def feed(i: Long): Program =
    copy(input = input.enqueue(i), state = state match {
      case Blocked => Running
      case other   => other
    })

  def extractOutput: (Program, Option[Long]) =
    output.dequeueOption
      .map { case (h, t) => (copy(output = t), h.some) }
      .getOrElse((this, None))

  def runOn(input: List[Long]): List[Long] =
    input.foldLeft(this) { case (p, i) => p.feed(i) }.run.output.toList

  def runFn(input: Long): Long =
    runOn(List(input)) match {
      case List(a) => a
    }
}

object Program {
  def parse(line: String): Program =
    Program(line.split(",").map(_.toLong).zipWithIndex.map { case (a, i) => (i.toLong, a) }.toMap)

  def feedS(input: Long): State[Program, Unit]       = State(p => (p.feed(input), ()))
  def feedAndRunS(input: Long): State[Program, Unit] = State(p => (p.feed(input).run, ()))
  val runS: State[Program, Unit]                     = State(p => (p.run, ()))
  val runToOutputS: State[Program, Option[Long]]     = State(_.runToOutput)
  val extractOutputS: State[Program, Option[Long]]   = State(_.extractOutput)

  implicit val showProgram: Show[Program] = {
    implicitly[Show[Array[Long]]] // this is required to help IntelliJ not delete cats/aoc imports
    derived.semi.show
  }

  sealed trait ProgramState
  object ProgramState {
    case object Running extends ProgramState
    case object Halted  extends ProgramState
    case object Blocked extends ProgramState
    implicit val eqState: Eq[ProgramState]     = Eq.fromUniversalEquals
    implicit val showState: Show[ProgramState] = Show.fromToString
  }
}

package aoc.intcode

import aoc.implicits._
import aoc.intcode.Instruction._
import aoc.intcode.Param._
import aoc.intcode.Program.State._
import cats.implicits._
import cats.{Eq, Show, derived}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

case class Program(
    memory: Array[Int],
    ip: Int = 0,
    state: Program.State = Running,
    input: Queue[Int] = Queue.empty,
    output: Queue[Int] = Queue.empty,
    debug: Boolean = false
) {
  def r(param: Param): Int =
    param match {
      case PositionParam(position) => memory(position)
      case ImmediateParam(value)   => value
    }
  def w(dest: PositionParam, value: Int): Program =
    write(dest.position, value)
  def in(dest: PositionParam): Program =
    input.dequeueOption match {
      case Some((h, t)) =>
        if (debug) println(s"IN> $h")
        w(dest, h).copy(input = t)
      case None =>
        if (debug) println("IN> no input, blocked")
        setState(Blocked)
    }

  def out(value: Int): Program = {
    if (debug) println(s"OUT> $value")
    copy(output = output.enqueue(value))
  }

  def move(n: Int): Program = {
    require(n > 0)
    copy(ip = ip + n)
  }
  def jump(position: Int): Program = {
    require(position >= 0)
    copy(ip = position)
  }
  def halt: Program =
    setState(Halted)

  private def setState(s: Program.State): Program = {
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

  def read(position: Int): Int =
    memory(position)

  def write(position: Int, value: Int): Program = {
    val updated = memory.clone
    updated(position) = value
    copy(memory = updated)
  }

  /** @param param 1-based */
  private def param(param: Int, ic: InstructionCode): Param = {
    val value = memory(ip + param)
    ic.parameterModes(param - 1) match {
      case 0 => // position mode
        PositionParam(value)
      case 1 => // immediate mode
        ImmediateParam(value)
    }
  }

  def step: Program = {
    assert(state === Running)
    val ic = InstructionCode.parse(memory(ip))
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
        case 99 =>
          HLT.some
      }
    val instruction = instructionO.getOrElse(throw new RuntimeException)
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

  def feed(i: Int): Program = {
    copy(input = input.enqueue(i), state = state match {
      case Blocked => Running
      case other   => other
    })
  }

  def extractOutput: (Option[Int], Program) =
    output.dequeueOption
      .map { case (h, t) => (h.some, copy(output = t)) }
      .getOrElse((None, this))

  def runOn(input: List[Int]): List[Int] =
    input.foldRight(this) { case (i, p) => p.feed(i) }.run.output.toList

  def runFn(input: Int): Int =
    runOn(List(input)) match {
      case List(a) => a
    }
}

object Program {
  def parse(line: String): Program =
    Program(line.split(",").map(_.toInt))

  implicit val showProgram: Show[Program] = {
    implicitly[Show[Array[Int]]] // this is required to help IntelliJ not delete cats/aoc imports
    derived.semi.show
  }

  sealed trait State
  object State {
    case object Running extends State
    case object Halted  extends State
    case object Blocked extends State
    implicit val eqState: Eq[State]     = Eq.fromUniversalEquals
    implicit val showState: Show[State] = Show.fromToString
  }
}

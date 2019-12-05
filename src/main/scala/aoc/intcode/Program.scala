package aoc.intcode

import aoc.implicits._
import aoc.intcode.Instruction._
import aoc.intcode.Param._
import cats.implicits._
import cats.{Show, derived}

import scala.annotation.tailrec

case class Program(
    memory: Array[Int],
    ip: Int = 0,
    halted: Boolean = false,
    input: List[Int] = Nil,
    output: List[Int] = Nil,
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
    w(dest, input.head).copy(input = input.tail)
  def out(value: Int): Program =
    copy(output = value :: output)
  def move(n: Int): Program = {
    require(n > 0)
    copy(ip = ip + n)
  }
  def jump(position: Int): Program = {
    require(position >= 0)
    copy(ip = position)
  }
  def halt: Program = {
    require(!halted)
    copy(halted = true)
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
    assert(!halted)
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
    if (debug)
      println(f"$ip%5s $instruction")
    instruction.apply(this)
  }

  @tailrec
  final def run: Program =
    if (halted) this
    else step.run

  def runOn(input: List[Int]): List[Int] =
    copy(input = input).run.output

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
}

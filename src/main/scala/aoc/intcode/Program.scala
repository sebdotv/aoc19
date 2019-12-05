package aoc.intcode

import aoc.implicits._
import cats.implicits._
import cats.{Show, derived}

import scala.annotation.tailrec

case class Program(
    memory: Array[Int],
    ip: Int = 0,
    halted: Boolean = false,
    input: List[Int] = Nil,
    output: List[Int] = Nil
) {
  def read(position: Int): Int =
    memory(position)

  def write(position: Int, value: Int): Program = {
    val updated = memory.clone
    updated(position) = value
    copy(memory = updated)
  }

  /** @param param 1-based */
  private def resolveParam(param: Int, parameterModes: Array[Int]): Int = {
    val value = memory(ip + param)
    parameterModes(param - 1) match {
      case 0 => // position mode
        memory(value)
      case 1 => // immediate mode
        value
    }
  }

  def step: Program = {
    assert(!halted)
    val ic = InstructionCode.parse(memory(ip))
    ic.opcode match {
      case 1 => // ADD
        assert(ic.parameterModes(2) === 0)
        write(memory(ip + 3), resolveParam(1, ic.parameterModes) + resolveParam(2, ic.parameterModes))
          .copy(ip = ip + 4)
      case 2 => // MUL
        assert(ic.parameterModes(2) === 0)
        write(memory(ip + 3), resolveParam(1, ic.parameterModes) * resolveParam(2, ic.parameterModes))
          .copy(ip = ip + 4)
      case 3 => // IN
        assert(ic.parameterModes === Array(0, 0, 0))
        write(memory(ip + 1), input.head)
          .copy(ip = ip + 2, input = input.tail)
      case 4 => // OUT
        assert(ic.parameterModes(1) === 0)
        assert(ic.parameterModes(2) === 0)
        copy(ip = ip + 2, output = resolveParam(1, ic.parameterModes) :: output)
      case 5 => // JNZ
        assert(ic.parameterModes(2) === 0)
        resolveParam(1, ic.parameterModes) match {
          case 0 => copy(ip = ip + 3) // nop
          case _ => copy(ip = resolveParam(2, ic.parameterModes))
        }
      case 6 => // JZ
        assert(ic.parameterModes(2) === 0)
        resolveParam(1, ic.parameterModes) match {
          case 0 => copy(ip = resolveParam(2, ic.parameterModes))
          case _ => copy(ip = ip + 3) // nop
        }
      case 7 => // LT
        assert(ic.parameterModes(2) === 0)
        write(
          memory(ip + 3),
          if (resolveParam(1, ic.parameterModes) < resolveParam(2, ic.parameterModes)) 1
          else 0
        ).copy(ip = ip + 4)
      case 8 => // EQ
        assert(ic.parameterModes(2) === 0)
        write(
          memory(ip + 3),
          if (resolveParam(1, ic.parameterModes) === resolveParam(2, ic.parameterModes)) 1
          else 0
        ).copy(ip = ip + 4)
      case 99 => // HLT
        assert(ic.parameterModes === Array(0, 0, 0))
        copy(halted = true)
    }
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

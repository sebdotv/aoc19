package aoc.intcode

import aoc._
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

  def step: Program = {
    assert(!halted)
    memory(ip) match {
      case 1 => // ADD
        write(memory(ip + 3), memory(memory(ip + 1)) + memory(memory(ip + 2)))
          .copy(ip = ip + 4)
      case 2 => // MUL
        write(memory(ip + 3), memory(memory(ip + 1)) * memory(memory(ip + 2)))
          .copy(ip = ip + 4)
      case 3 => // IN
        write(memory(ip + 1), input.head)
          .copy(ip = ip + 2, input = input.tail)
      case 4 => // OUT
        copy(ip = ip + 2, output = read(memory(ip + 1)) :: output)
      case 99 => // HLT
        copy(halted = true)
    }
  }

  @tailrec
  final def run: Program =
    if (halted) this
    else step.run

  def runOn(input: List[Int]): List[Int] =
    copy(input = input).run.output
}

object Program {
  def parse(line: String): Program =
    Program(line.split(",").map(_.toInt))

  implicit val showProgram: Show[Program] = {
    implicitly[Show[Array[Int]]] // this is required to help IntelliJ not delete cats/aoc imports
    derived.semi.show
  }
}

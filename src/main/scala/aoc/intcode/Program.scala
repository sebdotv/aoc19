package aoc.intcode

import aoc._
import cats.implicits._
import cats.{Show, derived}

import scala.annotation.tailrec

case class Program(
    memory: Array[Int],
    ip: Int = 0,
    halted: Boolean = false
) {
  def getMemory(position: Int): Int =
    memory(position)

  def setMemory(position: Int, value: Int): Program = {
    val updated = memory.clone
    updated(position) = value
    copy(memory = updated)
  }

  def patch(noun: Int, verb: Int): Program =
    setMemory(1, noun).setMemory(2, verb)

  def step: Program = {
    assert(!halted)
    memory(ip) match {
      case 1 =>
        setMemory(memory(ip + 3), memory(memory(ip + 1)) + memory(memory(ip + 2)))
          .copy(ip = ip + 4)
      case 2 =>
        setMemory(memory(ip + 3), memory(memory(ip + 1)) * memory(memory(ip + 2)))
          .copy(ip = ip + 4)
      case 99 =>
        copy(halted = true)
    }
  }

  @tailrec
  final def run: Program =
    if (halted) this
    else step.run
}

object Program {
  def parse(line: String): Program =
    Program(line.split(",").map(_.toInt))

  implicit val showProgram: Show[Program] = {
    implicitly[Show[Array[Int]]] // this is required to help IntelliJ not delete cats/aoc imports
    derived.semi.show
  }
}

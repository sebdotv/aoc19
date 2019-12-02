package d02

import cats.implicits._
import cats.data.State
import cats.effect.IO
import cats.{Show, derived}

import aoc._

import scala.annotation.tailrec

object d02 extends AocApp("input/02.txt") {
  override def program(lines: List[String]) =
    lines match {
      case List(line) =>
        stepUntilHalted(
          Program(line.split(",").map(_.toInt))
            .setMemory(1, 12)
            .setMemory(2, 2)
        ).getMemory(0)
    }

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
  }
  implicit val showProgram: Show[Program] = derived.semi.show

  val initialProgram = Program(Array(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50))
  @tailrec
  def stepUntilHalted(p: Program): Program =
    if (p.halted) p
    else stepUntilHalted(p.step)

  val endProgram = stepUntilHalted(initialProgram)
  println(endProgram.show)
}

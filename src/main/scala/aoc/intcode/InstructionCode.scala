package aoc.intcode

import aoc.implicits._
import cats.implicits._
import cats.{Show, derived}

case class InstructionCode private (
    opcode: Int,
    parameterModes: Array[Int]
) {
  require(parameterModes.size === 3)
}
object InstructionCode {
  def parse(i: Int) =
    InstructionCode(
      opcode = i % 100,
      parameterModes = (i / 100).toString.toArray.map(_ - '0').reverse.padTo(3, 0)
    )
  implicit val showInstructionCode: Show[InstructionCode] = {
    implicitly[Show[Array[Int]]] // this is required to help IntelliJ not delete cats/aoc imports
    derived.semi.show
  }
}

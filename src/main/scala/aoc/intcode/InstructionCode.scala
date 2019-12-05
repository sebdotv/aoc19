package aoc.intcode

import cats.implicits._

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
}

package aoc.intcode

import aoc.intcode.Program.State.Blocked
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must._

class ProgramSpec extends AnyFlatSpec with Matchers {
  "runOn" should "preserve input order" in {
    Program(Array(3, 0, 4, 0, 3, 0, 4, 0, 99)).runOn(List(1, 2)) mustBe List(1, 2)
  }
  it should "unblock programs" in {
    val p = Program(Array(3, 0, 4, 0, 99)).run
    p.state mustBe Blocked
    p.runOn(Nil) mustBe Nil

    p.runOn(List(1)) mustBe List(1)
  }
}

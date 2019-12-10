package aoc.trigo

import aoc.trigo.VectorUtils.simplifyVector
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class VectorUtilsSpec extends AnyFlatSpec with Matchers {
  "simplifyVector" should "find minimal vector" in {
    simplifyVector(Coord(2, 4)) mustBe Coord(1, 2)
    simplifyVector(Coord(6, 9)) mustBe Coord(2, 3)
    simplifyVector(Coord(-4, 2)) mustBe Coord(-2, 1)
    simplifyVector(Coord(4, -2)) mustBe Coord(2, -1)
  }
}

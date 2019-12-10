package d10

import aoc.geometry.Coord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class ReduceVectorSpec extends AnyFlatSpec with Matchers {
  "reduceVector" should "find minimal vector" in {
    import ReduceVector.reduce
    reduce(Coord(2, 4)) mustBe Coord(1, 2)
    reduce(Coord(6, 9)) mustBe Coord(2, 3)
    reduce(Coord(-4, 2)) mustBe Coord(-2, 1)
    reduce(Coord(4, -2)) mustBe Coord(2, -1)
  }
}

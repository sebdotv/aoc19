import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must._

class ChallengeSpec extends AnyFlatSpec with Matchers {
  "lazy coder" should "do d01" in {
    // examples
    d01.Part1.massToFuel(12) mustBe 2
    d01.Part1.massToFuel(14) mustBe 2
    d01.Part1.massToFuel(1969) mustBe 654
    d01.Part1.massToFuel(100756) mustBe 33583
    // input
    val input = load("input/01.txt")
    d01.Part1.result(input) mustBe 3560353
    d01.Part2.result(input) mustBe 5337642
  }

  it should "do d02" in {
    // example
    d02.Part1.parse("1,9,10,3,2,3,11,0,99,30,40,50").run.memory mustBe Array(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50)
    // input
    val input = d02.Part1.parse(loadLine("input/02.txt"))
    input.patch(noun = 12, verb = 2).run.getMemory(0) mustBe 3516593
    d02.Part2.result(input) mustBe 7749
  }

  private def load(filename: String)     = aoc.load(filename).unsafeRunSync()
  private def loadLine(filename: String) = aoc.loadLine(filename).unsafeRunSync()
}

import org.scalatest._
import org.scalatest.matchers.should._

class ChallengeSpec extends FlatSpec with Matchers {
  "lazy coder" should "do d01" in {
    d01.Part1.massToFuel(12) shouldBe 2
    d01.Part1.massToFuel(14) shouldBe 2
    d01.Part1.massToFuel(1969) shouldBe 654
    d01.Part1.massToFuel(100756) shouldBe 33583
    val input = load("input/01.txt")
    d01.Part2.result(input) shouldBe 5337642
  }

  it should "do d02" in {
    d02.Part1.parse("1,9,10,3,2,3,11,0,99,30,40,50").run.memory shouldBe
      Array(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50)
    val program = d02.Part1.parse(loadLine("input/02.txt"))
    program
      .patch(12, 2)
      .run
      .getMemory(0) shouldBe 3516593
    d02.Part2.result(program) shouldBe 7749
  }

  private def load(filename: String)     = aoc.load(filename).unsafeRunSync()
  private def loadLine(filename: String) = aoc.loadLine(filename).unsafeRunSync()
}

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

  it should "do d03" in {
    val List(path1, path2) = load("input/03.txt")
    // p1 examples
    d03.Part1.result("R8,U5,L5,D3", "U7,R6,D4,L4") mustBe 6
    d03.Part1.result("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83") mustBe 159
    d03.Part1.result("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") mustBe 135
    // p1 input
    d03.Part1.result(path1, path2) mustBe 1674
    // p2 examples
    d03.Part2.result("R8,U5,L5,D3", "U7,R6,D4,L4") mustBe 30
    d03.Part2.result("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83") mustBe 610
    d03.Part2.result("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") mustBe 410
    // p2 input
    d03.Part2.result(path1, path2) mustBe 14012
  }

  private def load(filename: String)     = aoc.load(filename).unsafeRunSync()
  private def loadLine(filename: String) = aoc.loadLine(filename).unsafeRunSync()
}

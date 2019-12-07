import TestUtils._
import aoc.intcode._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must._
import org.scalatest.Inside.inside

class ChallengeSpec extends AnyFlatSpec with Matchers {
  "lazy coder" should "do d01" in {
    import d01._
    // examples
    Part1.massToFuel(12) mustBe 2
    Part1.massToFuel(14) mustBe 2
    Part1.massToFuel(1969) mustBe 654
    Part1.massToFuel(100756) mustBe 33583
    // input
    val input = load("input/01.txt")
    Part1.result(input) mustBe 3560353
    Part2.result(input) mustBe 5337642
  }

  it should "do d02" in {
    import d02._
    // example
    Program.parse("1,9,10,3,2,3,11,0,99,30,40,50").run.memory mustBe Array(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50)
    // input
    val input = Program.parse(loadLine("input/02.txt"))
    Part1.patch(input, noun = 12, verb = 2).run.read(0) mustBe 3516593
    Part2.result(input) mustBe 7749
  }

  it should "do d03" in {
    import d03._
    val List(path1, path2) = load("input/03.txt")
    // p1 examples
    Part1.result("R8,U5,L5,D3", "U7,R6,D4,L4") mustBe 6
    Part1.result("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83") mustBe 159
    Part1.result("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") mustBe 135
    // p1 input
    Part1.result(path1, path2) mustBe 1674
    // p2 examples
    Part2.result("R8,U5,L5,D3", "U7,R6,D4,L4") mustBe 30
    Part2.result("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83") mustBe 610
    Part2.result("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") mustBe 410
    // p2 input
    Part2.result(path1, path2) mustBe 14012
  }

  it should "do d04" in {
    import d04._
    // p1 examples
    Part1.isValid(122345) mustBe true
    Part1.isValid(111123) mustBe true
    Part1.isValid(135679) mustBe false
    Part1.isValid(111111) mustBe true
    Part1.isValid(223450) mustBe false
    Part1.isValid(123789) mustBe false
    // p1 input
    val input = Part1.parseRange(loadLine("input/04.txt"))
    input.count(Part1.isValid) mustBe 895
    // p2 examples
    Part2.isValid(112233) mustBe true
    Part2.isValid(123444) mustBe false
    Part2.isValid(111122) mustBe true
    // p2 input
    input.count(Part2.isValid) mustBe 591
  }

  it should "do d05" in {
    import d05._
    // p1 examples
    Program.parse("3,0,4,0,99").runFn(12345) mustBe 12345
    inside(InstructionCode.parse(1002)) {
      case InstructionCode(opcode, parameterModes) =>
        opcode mustBe 2
        parameterModes mustBe Array(0, 1, 0)
    }
    Program.parse("1002,4,3,4,33").run.memory mustBe Array(1002, 4, 3, 4, 99)
    // p1 input
    val input = Program.parse(loadLine("input/05.txt"))
    Part1.result(input) mustBe List(6069343, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    // p2 examples
    Program.parse("3,9,8,9,10,9,4,9,99,-1,8") match {
      case p =>
        // pos_eq
        p.runFn(7) mustBe 0
        p.runFn(8) mustBe 1
    }
    Program.parse("3,9,7,9,10,9,4,9,99,-1,8") match {
      case p =>
        // pos_lt
        p.runFn(7) mustBe 1
        p.runFn(8) mustBe 0
    }
    Program.parse("3,3,1108,-1,8,3,4,3,99") match {
      case p =>
        // imm_eq
        p.runFn(7) mustBe 0
        p.runFn(8) mustBe 1
    }
    Program.parse("3,3,1107,-1,8,3,4,3,99") match {
      case p =>
        // imm_lt
        p.runFn(7) mustBe 1
        p.runFn(8) mustBe 0
    }
    Program.parse("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9") match {
      case p =>
        // pos_jmp
        p.runFn(0) mustBe 0
        p.runFn(1) mustBe 1
    }
    Program.parse("3,3,1105,-1,9,1101,0,0,12,4,12,99,1") match {
      case p =>
        // imm_jmp
        p.runFn(0) mustBe 0
        p.runFn(1) mustBe 1
    }
    // larger example
    Program.parse(
      "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
    ) match {
      case p =>
        p.runFn(7) mustBe 999
        p.runFn(8) mustBe 1000
        p.runFn(9) mustBe 1001
    }
    // p2 input
    Part2.result(input) mustBe 3188550
  }

  it should "do d06" in {
    def split(s: String) = s.trim.split("\n").toList
    import d06._
    // p1 examples
    val testInput1 = split("""
      |COM)B
      |B)C
      |C)D
      |D)E
      |E)F
      |B)G
      |G)H
      |D)I
      |E)J
      |J)K
      |K)L
      |""".stripMargin)
    val testOD1    = new Part1.OrbitDag(testInput1)
    testOD1.orbits("D") mustBe 3
    testOD1.orbits("L") mustBe 7
    testOD1.orbits("COM") mustBe 0
    testOD1.totalOrbits mustBe 42
    // p1 input
    val inputOD = new Part1.OrbitDag(load("input/06.txt"))
    inputOD.totalOrbits mustBe 251208
    // p2 examples
    val testInput2 = split("""
        |COM)B
        |B)C
        |C)D
        |D)E
        |E)F
        |B)G
        |G)H
        |D)I
        |E)J
        |J)K
        |K)L
        |K)YOU
        |I)SAN
        |""".stripMargin)
    Part2.minTransfers(new Part1.OrbitDag(testInput2), "YOU", "SAN") mustBe 4
    // p2 input
    Part2.minTransfers(inputOD, "YOU", "SAN") mustBe 397
  }

  it should "do d07" in {
    import d07._
    // p1 examples
    Part1.result("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0") mustBe ((List(4, 3, 2, 1, 0), 43210))
    Part1.result("3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0") mustBe ((List(0, 1, 2, 3, 4), 54321))
    Part1.result("3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0") mustBe (
      (
        List(1, 0, 4, 3, 2),
        65210
      )
    )
    // p1 input
    Part1.result(loadLine("input/07.txt")) mustBe ((List(4, 2, 1, 0, 3), 34686))
  }

}

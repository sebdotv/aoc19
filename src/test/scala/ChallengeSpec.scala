import TestUtils._
import aoc.geometry.Coord
import aoc.intcode._
import org.scalatest.Inside.inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must._

import scala.collection.immutable.Queue

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
    Program.parse("1,9,10,3,2,3,11,0,99,30,40,50").run.memory mustBe List(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50)
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
        parameterModes mustBe List(0, 1, 0)
    }
    Program.parse("1002,4,3,4,33").run.memory mustBe List(1002, 4, 3, 4, 99)
    // p1 input
    val input = Program.parse(loadLine("input/05.txt"))
    Part1.result(input) mustBe List(0, 0, 0, 0, 0, 0, 0, 0, 0, 6069343)
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
    import d06._
    // p1 examples
    val testInput1 = """
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
      |""".stripMargin.splitLines
    val testOD1    = new Part1.OrbitDag(testInput1)
    testOD1.orbits("D") mustBe 3
    testOD1.orbits("L") mustBe 7
    testOD1.orbits("COM") mustBe 0
    testOD1.totalOrbits mustBe 42
    // p1 input
    val inputOD = new Part1.OrbitDag(load("input/06.txt"))
    inputOD.totalOrbits mustBe 251208
    // p2 examples
    val testInput2 = """
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
        |""".stripMargin.splitLines
    Part2.minTransfers(new Part1.OrbitDag(testInput2), "YOU", "SAN") mustBe 4
    // p2 input
    Part2.minTransfers(inputOD, "YOU", "SAN") mustBe 397
  }

  it should "do d07" in {
    import d07._
    val input = loadLine("input/07.txt")
    // p1 examples
    Part1.result("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0") mustBe ((List(4, 3, 2, 1, 0), 43210))
    Part1.result("3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0") mustBe ((List(0, 1, 2, 3, 4), 54321))
    Part1.result("3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0") mustBe
      ((List(1, 0, 4, 3, 2), 65210))
    // p1 input
    Part1.result(input) mustBe ((List(4, 2, 1, 0, 3), 34686))
    // p2 examples
    Part2.result("3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5") mustBe
      ((List(9, 8, 7, 6, 5), 139629729))
    Part2.result(
      "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
    ) mustBe
      ((List(9, 7, 8, 5, 6), 18216))
    // p2 input
    Part2.result(input) mustBe ((List(7, 6, 5, 8, 9), 36384144))
  }

  it should "do d08" in {
    import d08._
    // p1 example
    inside(Part1.Image.parse("123456789012", 3, 2)) {
      case img =>
        img.w mustBe 3
        img.h mustBe 2
        img.layers mustBe 2
        img.pixel(0, 0, 0) mustBe 1
        img.pixel(1, 0, 0) mustBe 2
        img.pixel(0, 1, 0) mustBe 4
        img.pixel(0, 0, 1) mustBe 7
        img.pixel(1, 0, 1) mustBe 8
        img.pixel(0, 1, 1) mustBe 0
        img.layer(0).getPixels mustBe List(1, 2, 3, 4, 5, 6)
        img.layer(1).getPixels mustBe List(7, 8, 9, 0, 1, 2)
    }
    // p1 input
    val input      = loadLine("input/08.txt")
    val inputImage = Part1.Image.parse(input, 25, 6)
    Part1.result(inputImage) mustBe 2125
    // p2 examples
    import Part2.Color._
    Part2.renderPixel(
      new Part1.Image {
        override def w: Int      = 1
        override def h: Int      = 1
        override def layers: Int = 4
        override def pixel(x: Int, y: Int, z: Int): Int = (x, y) match {
          case (0, 0) =>
            z match {
              case 0 | 1 => Transparent
              case 2     => Black
              case 3     => White
            }
          case _ => throw new RuntimeException
        }
      },
      0,
      0
    ) mustBe Black
    Part2.renderImage(Part1.Image.parse("0222112222120000", 2, 2)).getPixels mustBe List(Black, White, White, Black)
    // p2 input
    Part2.dumpLayer(Part2.renderImage(inputImage), 0).trim mustBe
      """
        |  XX X   XXXXX X  X XXXX 
        |   X X   X   X X  X X    
        |   X  X X   X  XXXX XXX  
        |   X   X   X   X  X X    
        |X  X   X  X    X  X X    
        | XX    X  XXXX X  X X    
        |""".stripMargin.trim
  }

  it should "do d09" in {
    // p1 example 1
    val p1 = Program.parse("109,19,204,-34").copy(relativeBase = 2000).step
    p1.relativeBase mustBe 2019
    p1.write(1985, 123456).step.output mustBe Queue(123456)
    // p1 examples
    Program.parse("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99").run.output mustBe
      Queue(109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99)
    Program.parse("1102,34915192,34915192,7,4,7,99,0").run.output mustBe Queue(1219070632396864L)
    Program.parse("104,1125899906842624,99").run.output mustBe Queue(1125899906842624L)
    // p1 input
    val p = Program.parse(loadLine("input/09.txt"))
    p.runFn(1) mustBe 2870072642L
    // p2
    p.runFn(2) mustBe 58534
  }

  it should "do d10" in {
    import d10._
    inside(RegionMap.parse("""
        |.#..#
        |.....
        |#####
        |....#
        |...##
        |""".stripMargin.splitLines)) {
      case rm =>
        rm.w mustBe 5
        rm.h mustBe 5
        rm.asteroids(Coord(0, 0)) mustBe false
        rm.asteroids(Coord(1, 0)) mustBe true
        rm.asteroids(Coord(0, 1)) mustBe false
        rm.asteroids.size mustBe 10
    }
  }
}

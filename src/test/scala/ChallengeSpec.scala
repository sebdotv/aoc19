import TestUtils._
import aoc.intcode.Program.ProgramState.Halted
import aoc.intcode._
import aoc.trigo.Coord
import cats.implicits._
import org.scalatest.Inside.inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

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
    // p1 examples
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
        Part1.bestLocation(rm) mustBe ((Coord(3, 4), 8))
        Part1.detectionGrid(rm).trim mustBe
          """
            |.7..7
            |.....
            |67775
            |....7
            |...87
            |""".stripMargin.trim
    }
    Part1.bestLocation(
      RegionMap.parse("""
        |......#.#.
        |#..#.#....
        |..#######.
        |.#.#.###..
        |.#..#.....
        |..#....#.#
        |#..#....#.
        |.##.#..###
        |##...#..#.
        |.#....####
        |""".stripMargin.splitLines)
    ) mustBe ((Coord(5, 8), 33))
    Part1.bestLocation(
      RegionMap.parse("""
          |#.#...#.#.
          |.###....#.
          |.#....#...
          |##.#.#.#.#
          |....#.#.#.
          |.##..###.#
          |..#...##..
          |..##....##
          |......#...
          |.####.###.
          |""".stripMargin.splitLines)
    ) mustBe ((Coord(1, 2), 35))
    Part1.bestLocation(
      RegionMap.parse("""
          |.#..#..###
          |####.###.#
          |....###.#.
          |..###.##.#
          |##.##.#.#.
          |....###..#
          |..#.#..#.#
          |#..#.#.###
          |.##...##.#
          |.....#.#..
          |""".stripMargin.splitLines)
    ) mustBe ((Coord(6, 3), 41))
    val largeExample = RegionMap.parse("""
      |.#..##.###...#######
      |##.############..##.
      |.#.######.########.#
      |.###.#######.####.#.
      |#####.##.#.##.###.##
      |..#####..#.#########
      |####################
      |#.####....###.#.#.##
      |##.#################
      |#####.##.###..####..
      |..######..##.#######
      |####.##.####...##..#
      |.#####..#.######.###
      |##...#.##########...
      |#.##########.#######
      |.####.#.###.###.#.##
      |....##.##.###..#####
      |.#.#.###########.###
      |#.#.#.#####.####.###
      |###.##.####.##.#..##
      |""".stripMargin.splitLines)
    Part1.bestLocation(largeExample) mustBe ((Coord(11, 13), 210))
    // p1 input
    val input = RegionMap.parse(load("input/10.txt"))
    Part1.bestLocation(input) mustBe ((Coord(31, 20), 319))
    // p2 examples
    inside(RegionMap.parse("""
        |.#....#####...#..
        |##...##.#####..##
        |##...#...#.#####.
        |..#.....#...###..
        |..#.#.....#....##
        |""".stripMargin.splitLines)) {
      case rm =>
        val station = Part1.bestLocation(rm)._1
        station mustBe Coord(8, 3)
        val vaporizationOrder = Part2.vaporizationOrder(rm, station)
        Part2.laserGrid(rm, station, vaporizationOrder.take(9)).trim mustBe
          """
            |.#....###24...#..
            |##...##.13#67..9#
            |##...#...5.8####.
            |..#.....X...###..
            |..#.#.....#....##
            |""".stripMargin.trim
    }
    inside(largeExample) {
      case rm =>
        val station = Part1.bestLocation(rm)._1
        station mustBe Coord(11, 13)
        val vaporizationOrder = Part2.vaporizationOrder(rm, station)
        vaporizationOrder(199) mustBe Coord(8, 2)
    }
    // p2 input
    val station = Part1.bestLocation(input)._1
    station mustBe Coord(31, 20)
    val vaporizationOrder = Part2.vaporizationOrder(input, station)
    val v200              = vaporizationOrder(199)
    v200 mustBe Coord(5, 17)
    v200.x * 100 + v200.y mustBe 517
  }

  it should "do d11" in {
    import d11._
    val input = Part1.Region(Program.parse(loadLine("input/11.txt")))
    inside(input.run) {
      case r =>
        r.p.state mustBe Halted
        r.p.input mustBe empty
        r.p.output mustBe empty
        r.paintedPanels.size mustBe 1709
    }
    Part2.render(input.copy(paintedPanels = Map(input.robot -> 1L)).run.paintedPanels).trim mustBe
      """
        | ###   ##  #  # #### #  #  ##    ## #  #   
        | #  # #  # #  # #    #  # #  #    # #  #   
        | #  # #    #  # ###  #### #       # ####   
        | ###  # ## #  # #    #  # #       # #  #   
        | #    #  # #  # #    #  # #  # #  # #  #   
        | #     ###  ##  #### #  #  ##   ##  #  #   
        |""".stripMargin.trim
  }

  it should "do d12" in {
    import d12._
    import Part1._
    implicit class StringCleanupImprovements(s: String) {
      def cleanup: String = s.trim.replaceAll("""(=) +(-?\d)""", """$1$2""")
    }
    // p1 examples
    gravity(Coord3(3, 0, 0), Coord3(5, 0, 0)) mustBe Coord3(1, 0, 0)
    val example1 = System.parse(
      """
        |<x=-1, y=0, z=2>
        |<x=2, y=-10, z=-7>
        |<x=4, y=-8, z=8>
        |<x=3, y=5, z=-1>
        |""".stripMargin.splitLines
    )
    inside(example1.trace(10, identity)) {
      case ss =>
        ss.map(_.show).reverse.mkString("\n\n") mustBe
          """
          |After 0 steps:
          |pos=<x=-1, y=  0, z= 2>, vel=<x= 0, y= 0, z= 0>
          |pos=<x= 2, y=-10, z=-7>, vel=<x= 0, y= 0, z= 0>
          |pos=<x= 4, y= -8, z= 8>, vel=<x= 0, y= 0, z= 0>
          |pos=<x= 3, y=  5, z=-1>, vel=<x= 0, y= 0, z= 0>
          |
          |After 1 step:
          |pos=<x= 2, y=-1, z= 1>, vel=<x= 3, y=-1, z=-1>
          |pos=<x= 3, y=-7, z=-4>, vel=<x= 1, y= 3, z= 3>
          |pos=<x= 1, y=-7, z= 5>, vel=<x=-3, y= 1, z=-3>
          |pos=<x= 2, y= 2, z= 0>, vel=<x=-1, y=-3, z= 1>
          |
          |After 2 steps:
          |pos=<x= 5, y=-3, z=-1>, vel=<x= 3, y=-2, z=-2>
          |pos=<x= 1, y=-2, z= 2>, vel=<x=-2, y= 5, z= 6>
          |pos=<x= 1, y=-4, z=-1>, vel=<x= 0, y= 3, z=-6>
          |pos=<x= 1, y=-4, z= 2>, vel=<x=-1, y=-6, z= 2>
          |
          |After 3 steps:
          |pos=<x= 5, y=-6, z=-1>, vel=<x= 0, y=-3, z= 0>
          |pos=<x= 0, y= 0, z= 6>, vel=<x=-1, y= 2, z= 4>
          |pos=<x= 2, y= 1, z=-5>, vel=<x= 1, y= 5, z=-4>
          |pos=<x= 1, y=-8, z= 2>, vel=<x= 0, y=-4, z= 0>
          |
          |After 4 steps:
          |pos=<x= 2, y=-8, z= 0>, vel=<x=-3, y=-2, z= 1>
          |pos=<x= 2, y= 1, z= 7>, vel=<x= 2, y= 1, z= 1>
          |pos=<x= 2, y= 3, z=-6>, vel=<x= 0, y= 2, z=-1>
          |pos=<x= 2, y=-9, z= 1>, vel=<x= 1, y=-1, z=-1>
          |
          |After 5 steps:
          |pos=<x=-1, y=-9, z= 2>, vel=<x=-3, y=-1, z= 2>
          |pos=<x= 4, y= 1, z= 5>, vel=<x= 2, y= 0, z=-2>
          |pos=<x= 2, y= 2, z=-4>, vel=<x= 0, y=-1, z= 2>
          |pos=<x= 3, y=-7, z=-1>, vel=<x= 1, y= 2, z=-2>
          |
          |After 6 steps:
          |pos=<x=-1, y=-7, z= 3>, vel=<x= 0, y= 2, z= 1>
          |pos=<x= 3, y= 0, z= 0>, vel=<x=-1, y=-1, z=-5>
          |pos=<x= 3, y=-2, z= 1>, vel=<x= 1, y=-4, z= 5>
          |pos=<x= 3, y=-4, z=-2>, vel=<x= 0, y= 3, z=-1>
          |
          |After 7 steps:
          |pos=<x= 2, y=-2, z= 1>, vel=<x= 3, y= 5, z=-2>
          |pos=<x= 1, y=-4, z=-4>, vel=<x=-2, y=-4, z=-4>
          |pos=<x= 3, y=-7, z= 5>, vel=<x= 0, y=-5, z= 4>
          |pos=<x= 2, y= 0, z= 0>, vel=<x=-1, y= 4, z= 2>
          |
          |After 8 steps:
          |pos=<x= 5, y= 2, z=-2>, vel=<x= 3, y= 4, z=-3>
          |pos=<x= 2, y=-7, z=-5>, vel=<x= 1, y=-3, z=-1>
          |pos=<x= 0, y=-9, z= 6>, vel=<x=-3, y=-2, z= 1>
          |pos=<x= 1, y= 1, z= 3>, vel=<x=-1, y= 1, z= 3>
          |
          |After 9 steps:
          |pos=<x= 5, y= 3, z=-4>, vel=<x= 0, y= 1, z=-2>
          |pos=<x= 2, y=-9, z=-3>, vel=<x= 0, y=-2, z= 2>
          |pos=<x= 0, y=-8, z= 4>, vel=<x= 0, y= 1, z=-2>
          |pos=<x= 1, y= 1, z= 5>, vel=<x= 0, y= 0, z= 2>
          |
          |After 10 steps:
          |pos=<x= 2, y= 1, z=-3>, vel=<x=-3, y=-2, z= 1>
          |pos=<x= 1, y=-8, z= 0>, vel=<x=-1, y= 1, z= 3>
          |pos=<x= 3, y=-6, z= 1>, vel=<x= 3, y= 2, z=-3>
          |pos=<x= 2, y= 0, z= 4>, vel=<x= 1, y=-1, z=-1>
          |""".stripMargin.cleanup
        val s10 = ss.head
        s10.steps mustBe 10
        s10.totalEnergy mustBe 179
    }
    val example2 = System.parse(
      """
        |<x=-8, y=-10, z=0>
        |<x=5, y=5, z=10>
        |<x=2, y=-7, z=3>
        |<x=9, y=-8, z=-3>
        |""".stripMargin.splitLines
    )
    inside(example2.run(100)) {
      case s =>
        s.show mustBe
          """
          |After 100 steps:
          |pos=<x=  8, y=-12, z= -9>, vel=<x= -7, y=  3, z=  0>
          |pos=<x= 13, y= 16, z= -3>, vel=<x=  3, y=-11, z= -5>
          |pos=<x=-29, y=-11, z= -1>, vel=<x= -3, y=  7, z=  4>
          |pos=<x= 16, y=-13, z= 23>, vel=<x=  7, y=  1, z=  1>
          |""".stripMargin.cleanup
        s.totalEnergy mustBe 1940
    }
    // p1 input
    val input = System.parse(load("input/12.txt"))
    input.run(1000).totalEnergy mustBe 12490
    // p2 examples
    inside(Part2.findRepetition(example1)) {
      case (a, b) =>
        a mustBe 0
        b mustBe 2772
    }
    Part2.result(example1) mustBe 2772
    Part2.result(example2) mustBe 4686774924L
    // p2 input
    Part2.result(input) mustBe 392733896255168L
  }

  it should "do d13" in {
    import d13._
    val input = ArcadeCabinet(Program.parse(loadLine("input/13.txt")))
    // p1
    Part1.countBlocks(input) mustBe 205
    // p2
    val a = input.copy(p = input.p.write(0, 2))
    Part2.run(a).screen.segmentDisplay mustBe Some(10292)
  }

  it should "do d14" in {
    import d14._
    inside(Part1.solve("""
          |10 ORE => 10 A
          |1 ORE => 1 B
          |7 A, 1 B => 1 C
          |7 A, 1 C => 1 D
          |7 A, 1 D => 1 E
          |7 A, 1 E => 1 FUEL
          |""".stripMargin.splitLines)) {
      case (ores, byProducts) =>
        ores mustBe 31
        byProducts mustBe Map("A" -> 2)
    }
    inside(Part1.solve("""
          |9 ORE => 2 A
          |8 ORE => 3 B
          |7 ORE => 5 C
          |3 A, 4 B => 1 AB
          |5 B, 7 C => 1 BC
          |4 C, 1 A => 1 CA
          |2 AB, 3 BC, 4 CA => 1 FUEL
          |""".stripMargin.splitLines)) {
      case (ores, _) =>
        ores mustBe 165
    }
    val example1 = NanoFactory.parse("""
      |157 ORE => 5 NZVS
      |165 ORE => 6 DCFZ
      |44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
      |12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
      |179 ORE => 7 PSHF
      |177 ORE => 5 HKGWZ
      |7 DCFZ, 7 PSHF => 2 XJWVT
      |165 ORE => 2 GPVTF
      |3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT
      |""".stripMargin.splitLines)
    inside(Part1.solve(example1)) {
      case (ores, _) =>
        ores mustBe 13312
    }
    val example2 = NanoFactory.parse("""
      |2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
      |17 NVRVD, 3 JNWZP => 8 VPVL
      |53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
      |22 VJHF, 37 MNCFX => 5 FWMGM
      |139 ORE => 4 NVRVD
      |144 ORE => 7 JNWZP
      |5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
      |5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
      |145 ORE => 6 MNCFX
      |1 NVRVD => 8 CXFTF
      |1 VJHF, 6 MNCFX => 4 RFSQX
      |176 ORE => 6 VJHF
      |""".stripMargin.splitLines)
    inside(Part1.solve(example2)) {
      case (ores, _) =>
        ores mustBe 180697
    }
    val example3 = NanoFactory.parse("""
      |171 ORE => 8 CNZTR
      |7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
      |114 ORE => 4 BHXH
      |14 VRPVC => 6 BMBT
      |6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
      |6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
      |15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
      |13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
      |5 BMBT => 4 WPTQ
      |189 ORE => 9 KTJDG
      |1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
      |12 VRPVC, 27 CNZTR => 2 XDBXC
      |15 KTJDG, 12 BHXH => 5 XCVML
      |3 BHXH, 2 VRPVC => 7 MZWV
      |121 ORE => 7 VRPVC
      |7 XCVML => 6 RJRHP
      |5 BHXH, 4 VRPVC => 5 LTCX
      |""".stripMargin.splitLines)
    inside(Part1.solve(example3)) {
      case (ores, _) =>
        ores mustBe 2210736
    }
    // p1 input
    val input = NanoFactory.parse(load("input/14.txt"))
    inside(Part1.solve(input)) {
      case (ores, _) =>
        ores mustBe 504284
    }
    // p2 examples
    val ores = 1000000000000L
    Part2.maxFuel(13312, Solver1(example1, SolverState().addCargo("ORE", ores))) mustBe 82892753
    Part2.maxFuel(180697, Solver1(example2, SolverState().addCargo("ORE", ores))) mustBe 5586022
    Part2.maxFuel(2210736, Solver1(example3, SolverState().addCargo("ORE", ores))) mustBe 460664
    // p2 input
    Part2.maxFuel(504284, Solver1(input, SolverState().addCargo("ORE", ores))) mustBe 2690795
  }
}

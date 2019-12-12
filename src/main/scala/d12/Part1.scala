package d12

import cats.data.State
import cats.implicits._
import cats.{Eq, Show}

import scala.annotation.tailrec

object Part1 {
  def gravity(a: Coord3, b: Coord3): Coord3 = (b - a).map(math.signum)

  case class Coord3(x: Int, y: Int, z: Int) {
    def +(other: Coord3)   = Coord3(x = x + other.x, y = y + other.y, z = z + other.z)
    def -(other: Coord3)   = Coord3(x = x - other.x, y = y - other.y, z = z - other.z)
    def map(f: Int => Int) = Coord3(x = f(x), y = f(y), z = f(z))
    def sum                = x + y + z
  }
  object Coord3 {
    val zero                              = Coord3(0, 0, 0)
    implicit val eqCoord3: Eq[Coord3]     = Eq.fromUniversalEquals
    implicit val showCoord3: Show[Coord3] = Show.show(a => s"<x=${a.x}, y=${a.y}, z=${a.z}>")
  }

  case class Moon(pos: Coord3, vel: Coord3 = Coord3.zero) {
    def potentialEnergy = pos.map(math.abs).sum
    def kineticEnergy   = vel.map(math.abs).sum
    def totalEnergy     = potentialEnergy * kineticEnergy
  }
  object Moon {
    def parse(line: String): Moon = {
      val MoonRegex          = """<x=(-?\d+), y=(-?\d+), z=(-?\d+)>""".r
      val MoonRegex(x, y, z) = line
      Moon(Coord3(x.toInt, y.toInt, z.toInt))
    }

    implicit val eqMoon: Eq[Moon]     = Eq.fromUniversalEquals
    implicit val showMoon: Show[Moon] = Show.show(a => show"pos=${a.pos}, vel=${a.vel}")
  }

  case class System(moons: List[Moon], steps: Int = 0) {
    def totalEnergy = moons.map(_.totalEnergy).sum

    def step: System = {
      def computeGravity(a: Moon): Coord3 =
        (for {
          b <- moons if b =!= a
        } yield gravity(a.pos, b.pos))
          .foldRight(Coord3.zero)(_ + _)

      val applyGravity: State[List[Moon], Unit] =
        State(moons => (moons.map(m => m.copy(vel = m.vel + computeGravity(m))), ()))
      val applyVelocity: State[List[Moon], Unit] =
        State(moons => (moons.map(m => m.copy(pos = m.pos + m.vel)), ()))
      val state =
        for {
          _ <- applyGravity
          _ <- applyVelocity
        } yield ()
      copy(steps = steps + 1, moons = state.run(moons).value._1)
    }

    @tailrec
    final def trace[A](steps: Int, f: System => A, acc: List[A] = Nil): List[A] =
      if (steps < 0) acc
      else step.trace(steps - 1, f, f(this) :: acc)
    @tailrec
    final def run(steps: Int): System =
      if (steps === 0) this
      else step.run(steps - 1)
  }
  object System {
    def parse(lines: List[String])  = System(lines.map(Moon.parse))
    def apply(moons: Moon*): System = System(moons.toList)
    implicit val showSystem: Show[System] =
      Show.show(a => (s"After ${a.steps} step${if (a.steps =!= 1) "s" else ""}:" :: a.moons.map(_.show)).mkString("\n"))

  }
}

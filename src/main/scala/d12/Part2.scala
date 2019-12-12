package d12

import aoc.math.lcm
import cats.Eq
import cats.data.State
import cats.implicits._
import d12.Part1._

import scala.annotation.tailrec

object Part2 {
  def findRepetition(s: System): (Int, Int) =
    findRepetitionG[System, Moon](s)

  def result(system: System): Long = {
    val periods =
      for (f <- List[Coord3 => Int](_.x, _.y, _.z))
        yield Part2.OneD
          .findRepetition(
            Part2.OneD.System.toOneD(system, f)
          )
          ._2
    periods.map(_.toLong).foldRight(1L)(lcm)
  }
  @tailrec
  def findRepetitionG[S <: SystemLike[M], M](s: S, history: Map[List[M], Int] = Map.empty[List[M], Int]): (Int, Int) =
    history.get(s.moons) match {
      case Some(previous) => (previous, s.steps)
      case None           => findRepetitionG(s.step, history + (s.moons -> s.steps))
    }

  object OneD {
    def findRepetition(s: System): (Int, Int) =
      findRepetitionG[System, Moon](s)

    def gravity(a: Int, b: Int): Int = math.signum(b - a)
    case class Moon(pos: Int, vel: Int = 0)
    object Moon {
      implicit val eqMoon: Eq[Moon] = Eq.fromUniversalEquals
    }

    case class System(moons: List[Moon], steps: Int = 0) extends SystemLike[Moon] {
      def step: System = {
        def computeGravity(a: Moon): Int =
          (for {
            b <- moons if b =!= a
          } yield gravity(a.pos, b.pos))
            .foldRight(0)(_ + _)

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
      def toOneD(s: Part1.System, f: Coord3 => Int) = System(
        moons = s.moons.map(m => Moon(pos = f(m.pos), vel = f(m.vel))),
        steps = s.steps
      )
    }
  }
}

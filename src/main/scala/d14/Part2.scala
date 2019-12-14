package d14

import scala.annotation.tailrec

import cats.implicits._

object Part2 {
  case class Solver2(f: NanoFactory, state: SolverState = SolverState(), done: Boolean = false) {
    def addCargo(chemical: String, n: Long = 1) = copy(state = state.addCargo(chemical, n))
    @tailrec
    final def run: Solver2 =
      if (done) this else step.run
    def step: Solver2 = {
      val fits = state.balances.toList.filter(_._2 > 0).flatMap {
        case (c, q) =>
          f.index.get(c).map(r => r -> q % r.n)
      }
      fits match {
        case Nil => copy(done = true)
        case _ =>
          val r = fits.minBy(_._2)._1
          copy(state = state.apply(r))
      }
    }
    override def toString: String = state.toString
  }

  @tailrec
  def maxFuel(opf: Long, s: Solver1, fuel: Long = 0): Long = {
    val remaining  = s.state.balances.getOrElse("ORE", 0L)
    val wantedFuel = remaining / opf
    if (wantedFuel === 0) fuel
    else
      maxFuel(opf, s.addWanted("FUEL", wantedFuel).run, fuel + wantedFuel)
  }
}

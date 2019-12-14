package d14

import cats.implicits._

import scala.annotation.tailrec

case class Reaction(n: Long, out: String, in: Map[String, Long])
object Reaction {
  private val ReactionRe   = """(.*) => (.*)""".r
  private val ChemicalQtRe = """(\d+) (\w+)""".r
  def parse(s: String): Reaction = {
    val ReactionRe(r, l) = s
    val in = r
      .split(", ")
      .map(parseChemicalQt)
      .groupBy(_._2)
      .mapValues(_.map(_._1).toList match { case List(n) => n })
    val (n, out) = parseChemicalQt(l)
    Reaction(n, out, in)
  }

  def parseChemicalQt(s: String): (Long, String) = {
    val ChemicalQtRe(n, chemical) = s
    (n.toLong, chemical)
  }
}

case class SolverState(balances: Map[String, Long] = Map.empty) {
  def addBalances(ms: Map[String, Long]*): SolverState =
    copy(balances = merge(balances :: ms.toList))
  def addWanted(chemical: String, n: Long = 1): SolverState =
    addBalances(Map(chemical -> -n))
  def apply(r: Reaction, n: Long = 1): SolverState = {
    require(n > 0, n)
    addBalances(
      r.in.mapValues(_ * -1 * n),
      Map(r.out -> r.n * n)
    )
  }

  private def merge(ms: List[Map[String, Long]]): Map[String, Long] =
    ms.map(_.toSeq)
      .foldRight(Seq.empty[(String, Long)])(_ ++ _)
      .groupBy(_._1)
      .mapValues(_.map(_._2).sum)
      .filterNot(_._2 === 0)

  override def toString: String = balances.toString
}

case class Solver1(f: NanoFactory, state: SolverState = SolverState(), done: Boolean = false) {
  def addWanted(chemical: String, n: Long = 1) = copy(state = state.addWanted(chemical, n))
  @tailrec
  final def run: Solver1 =
    if (done) this else step.run
  def step: Solver1 = {
    val fits = state.balances.toList.filter(_._2 < 0).flatMap {
      case (c, q) =>
        f.index.get(c).map(r => r -> -q % r.n)
    }
    fits match {
      case Nil => copy(done = true)
      case _ =>
        val r = fits.minBy(_._2)._1
        val q          = state.balances(r.out)
        val multiplier = (-q + r.n - 1) / r.n
        copy(state = state.apply(r, multiplier))
    }
  }
  override def toString: String = state.toString
}

case class NanoFactory(reactions: List[Reaction]) {
  val index = reactions.groupBy(_.out).mapValues { case List(r) => r }
}
object NanoFactory {
  def parse(lines: List[String]): NanoFactory =
    NanoFactory(lines.map(Reaction.parse))
}

object Part1 {
  def solve(lines: List[String]): (Long, Map[String, Long]) =
    solve(NanoFactory.parse(lines))
  def solve(f: NanoFactory): (Long, Map[String, Long]) = {
    val m = Solver1(f).addWanted("FUEL").run.state.balances
    (-m("ORE"), m - "ORE")
  }
}

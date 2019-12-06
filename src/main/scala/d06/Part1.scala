package d06

import cats.implicits._
import cats.kernel.Eq

import scala.annotation.tailrec

object Part1 {

  case class V(name: String) extends AnyVal
  case class E(from: V, to: V)
  implicit val eqV: Eq[V] = Eq.fromUniversalEquals
  implicit val eqE: Eq[E] = Eq.fromUniversalEquals

  class OrbitDag(input: List[String]) {
    val edges: Set[E] = (for (line <- input) yield {
      val Array(a, b) = line.split("""\)""")
      E(from = V(b), to = V(a))
    }).toSet
    val vertices: Set[V] = (for (e <- edges) yield Set(e.from, e.to)).flatten
    def outgoing(from: V): Option[E] =
      edges.filter(_.from === from).toList match {
        case List(a) => Some(a)
        case Nil     => None
        // assume max 1 outgoing edge for now
      }
    def orbits(vertexName: String): Int =
      orbitCount(V(vertexName))
    def orbitCount(v: V): Int =
      pathToRoot(v).size

    def pathToRoot(v: V): List[V] = {
      @tailrec
      def it(v: V, acc: List[V]): List[V] =
        outgoing(v) match {
          case Some(e) => it(e.to, e.to :: acc)
          case None    => acc
        }
      it(v, Nil)
    }
    def totalOrbits: Int =
      (for (v <- vertices.toList) yield orbitCount(v)).sum

  }

}

package aoc

import cats.Eq

trait EqInstances {
  implicit def eqArray[A: Eq]: Eq[Array[A]] = Eq.instance { case (a, b) => a.sameElements(b) }
}

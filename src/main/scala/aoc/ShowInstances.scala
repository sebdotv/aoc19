package aoc

import cats.Show
import cats.implicits._

trait ShowInstances {
  implicit def showArray[A: Show]: Show[Array[A]] = Show.show(a => s"[${a.map(_.show).mkString(", ")}]")
}

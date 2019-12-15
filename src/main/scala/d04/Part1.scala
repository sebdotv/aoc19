package d04

import cats.implicits._

object Part1 {
  def isValid(n: Int): Boolean = {
    val s       = n.toString
    val bigrams = s.zip(s.drop(1))
    bigrams.forall { case (a, b) => a <= b } && bigrams.exists { case (a, b) => a === b }
  }

  def parseRange(input: String): Range = {
    val Array(minS, maxS) = input.split("-")
    minS.toInt to maxS.toInt
  }
}

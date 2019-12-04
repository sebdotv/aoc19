package d04

object Part1 {
  def isValid(n: Int): Boolean = {
    val s       = n.toString
    val bigrams = s.zip(s.drop(1))
    bigrams.forall { case (a, b) => a <= b } && bigrams.find { case (a, b) => a == b }.isDefined
  }

  def parseRange(input: String): Range = {
    val Array(minS, maxS) = input.split("-")
    minS.toInt to maxS.toInt
  }
}

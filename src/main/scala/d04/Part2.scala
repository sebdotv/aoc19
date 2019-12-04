package d04

object Part2 {
  def isValid(n: Int): Boolean =
    Part1.isValid(n) &&
      n.toString.toSeq.groupBy(identity).values.find(_.size == 2).isDefined
}

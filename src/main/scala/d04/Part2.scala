package d04

object Part2 {
  def isValid(n: Int): Boolean =
    Part1.isValid(n) &&
      n.toString.toSeq.groupBy(identity).values.exists(_.size == 2)
}

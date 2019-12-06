package d06

import Part1._

object Part2 {
  def minTransfers(od: OrbitDag, name1: String, name2: String): Int = {
    minTransfers(
      od.pathToRoot(V(name1)).reverse,
      od.pathToRoot(V(name2)).reverse
    )
  }

  def minTransfers(rpath1: List[V], rpath2: List[V]): Int = {
    val path2M = rpath2.zipWithIndex
      .groupBy(_._1)
      .mapValues { case List((_, i)) => i }
    val Some((intersect, i)) = rpath1.zipWithIndex
      .find { case (v1, _) => path2M.contains(v1) }
    i + path2M(intersect)
  }
}

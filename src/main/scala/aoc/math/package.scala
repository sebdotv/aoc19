package aoc

import cats.implicits._
import scala.annotation.tailrec

package object math {
  @tailrec
  // Euclid's algorithm
  // ref: https://en.wikibooks.org/wiki/Undergraduate_Mathematics/Greatest_common_divisor#Using_Euclid's_algorithm
  def gcd(a: Int, b: Int): Int =
    if (b === 0) a
    else gcd(b, a % b)

  def lcm(a: Long, b: Long): Long =
    a * b / gcd(a.toInt, b.toInt)
}

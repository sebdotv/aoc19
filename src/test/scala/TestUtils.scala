object TestUtils {
  def load(filename: String): List[String] = aoc.load(filename).unsafeRunSync()
  def loadLine(filename: String): String   = aoc.loadLine(filename).unsafeRunSync()
}

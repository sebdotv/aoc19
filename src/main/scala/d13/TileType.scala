package d13

import cats.Eq
import cats.implicits._

sealed abstract case class TileType(id: Int)
object TileType {
  object Empty  extends TileType(0)
  object Wall   extends TileType(1)
  object Block  extends TileType(2)
  object Paddle extends TileType(3)
  object Ball   extends TileType(4)
  val values                            = List(Empty, Wall, Block, Paddle, Ball)
  def fromId(id: Int)                   = values.find(_.id === id).get
  implicit val eqTileType: Eq[TileType] = Eq.fromUniversalEquals
}

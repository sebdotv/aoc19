package d08

import cats.implicits._

object Part1 {
  def result(image: MultiLayerImage): Int = {
    val layers = for {
      z <- 0 until image.layers
    } yield image.layer(z)
    val min0sLayer = layers.minBy(_.getPixels.count(_ === 0))
    val pixels     = min0sLayer.getPixels
    pixels.count(_ === 1) * pixels.count(_ === 2)
  }

  sealed trait Image {
    def w: Int
    def h: Int
    def layers: Int
    def pixel(x: Int, y: Int, z: Int): Int
    def getPixels: List[Int] =
      (for {
        z <- 0 until layers
        y <- 0 until h
        x <- 0 until w
      } yield pixel(x, y, z)).toList
  }
  case class MultiLayerImage(w: Int, h: Int, layers: Int, private val pixels: Array[Int]) extends Image { self =>
    require(layers > 1)
    def pixel(x: Int, y: Int, z: Int) =
      pixels(x + (y + z * h) * w)
    def layer(z: Int): Image =
      new Image {
        override def w: Int      = self.w
        override def h: Int      = self.h
        override def layers: Int = 1
        override def pixel(x: Int, y: Int, nullZ: Int): Int = {
          require(nullZ === 0)
          self.pixel(x, y, z)
        }
      }
  }
  object Image {
    def parse(line: String, w: Int, h: Int): MultiLayerImage = {
      val pixels: Array[Int] = line.toCharArray.map(_ - '0')
      val layers             = pixels.size / (w * h)
      assert(layers * w * h === pixels.size)
      MultiLayerImage(w = w, h = h, layers = layers, pixels)
    }
  }

}

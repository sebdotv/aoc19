package d08

import d08.Part1.Image
import cats.implicits._

object Part2 {
  object Color {
    val Black       = 0
    val White       = 1
    val Transparent = 2
  }
  import Color._

  def renderPixel(image: Image, x: Int, y: Int): Int = {
    val pixels = for {
      z <- (0 until image.layers).toStream
    } yield image.pixel(x, y, z)
    val Some(color) = pixels.find(_ =!= Transparent)
    color
  }

  def renderImage(image: Image): Image =
    new Image {
      override def w: Int      = image.w
      override def h: Int      = image.h
      override def layers: Int = 1
      override def pixel(x: Int, y: Int, z: Int): Int = {
        require(z === 0)
        renderPixel(image, x, y)
      }
    }
}

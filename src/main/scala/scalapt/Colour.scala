package scalapt

import scala.math._

/**
  * A triple of colour coefficients.
  */
object RGB {

  object Black extends RGB(0.0, 0.0, 0.0)
  object White extends RGB(1.0, 1.0, 1.0)

  object Red   extends RGB(1.0, 0.0, 0.0)
  object Green extends RGB(0.0, 1.0, 0.0)
  object Blue  extends RGB(0.0, 0.0, 1.0)
}

case class RGB(red: Double, green: Double, blue: Double) {


  private def map(f: Double => Double): RGB = RGB(f(red), f(green), f(blue))


  def +(that: RGB) = RGB(this.red + that.red, this.green + that.green, this.blue + that.blue)

  def -(that: RGB) = RGB(this.red - that.red, this.green - that.green, this.blue - that.blue)

  def *(that: RGB) = RGB(this.red * that.red, this.green * that.green, this.blue * that.blue)

  def *(d: Double) = RGB(red * d, green * d, blue * d)

  def /(d: Double) = this * (1.0 / d)

  def hasNaN = red.isNaN || green.isNaN || blue.isNaN

  def max = math.max(math.max(red, green), blue)

  def clamp = map(x => Util.clamp(x))

  def toLinear(): RGB = map(x => pow(x, 2.2))

  def toGamma(): RGB = map(Gamma)

  val Gamma = (x: Double) => pow(x, 1.0 / 2.2)

  def outputColour(): Int = {
    val g = this.toGamma()

    // maps a 0.0-1.0 double color to 0-256 integer color
    def to8bit = (d: Double) => (d * 255.0 + 0.5).toInt // Util.clamp(val, 0, 255)

    to8bit(g.blue) | (to8bit(g.green) << 8) | (to8bit(g.red) << 16)
  }

  override def toString = "{R: " + red + ", G: " + green + ", B: " + blue + "}"

}

/**
  * SuperSamp is a 2x2 grid of colours, used for super-sampling.
  */
object SuperSamp {
  object Black extends SuperSamp(RGB.Black, RGB.Black, RGB.Black, RGB.Black)
}

case class SuperSamp(c00: RGB, c10: RGB, c01: RGB, c11: RGB) {

  def merge(that: SuperSamp, n: Int): SuperSamp =
    SuperSamp(
      (c00 * n + that.c00) / (n + 1),
      (c10 * n + that.c10) / (n + 1),
      (c01 * n + that.c01) / (n + 1),
      (c11 * n + that.c11) / (n + 1)
    )

  def clamp: RGB = (c00.clamp + c10.clamp + c01.clamp + c11.clamp) * 0.25

  def +(that: SuperSamp): SuperSamp =
    SuperSamp(c00 + that.c00, c10 + that.c10, c01 + that.c01, c11 + that.c11)
}

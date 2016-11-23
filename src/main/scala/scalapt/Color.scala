package scalapt

import scala.math._

/**
  * A triple of color coefficients.
  */
object RGB {

  val Black = RGB(0.0, 0.0, 0.0)
  val White = RGB(1.0, 1.0, 1.0)

  val Red   = RGB(1.0, 0.0, 0.0)
  val Green = RGB(0.0, 1.0, 0.0)
  val Blue  = RGB(0.0, 0.0, 1.0)
}

// three doubles in the unit interval (0,1) representing the portion of unabsorbed light in that band.
// so RGB(0.0, 1.0, 0.5) represents 0% red, 100% green, and 50% blue of a light source/ray/etc,
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

  // this calculates the running average, given that this rgb represents n samples/ is weighted by n
  def merge(that: RGB, n: Double) = (this * n + that) / (n + 1)

  //def toLinear: RGB = map(x => pow(x, 2.2))

  // converts to gamma space, for display
  def toGamma: RGB = map(x => pow(x, 1.0 / 2.2))

  // converts to SRGB space
  def toSRGB: RGB = map(x => {
    if (x > 0.0031308)
      1.055 * pow(x, 1.0/2.4) - 0.055
    else
      x * 12.92
  })

  def outputColour: Int = {
    val g = this.toSRGB

    // maps a 0.0-1.0 double color to 0-256 integer color
    def to8bit = (d: Double) => (d * 255.0 + 0.5).toInt // Util.clamp(val, 0, 256)

    to8bit(g.blue) | (to8bit(g.green) << 8) | (to8bit(g.red) << 16)
  }

  override def toString = "{R: " + red + ", G: " + green + ", B: " + blue + "}"

}

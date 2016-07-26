package scalapt

/**
  * A triple of colour coefficients.
  */
object RGB {
  final val black = RGB(0.0, 0.0, 0.0)
  final val white = RGB(1.0, 1.0, 1.0)

  final val red   = RGB(1.0, 0.0, 0.0)
  final val green = RGB(0.0, 1.0, 0.0)
  final val blue  = RGB(0.0, 0.0, 1.0)

  //def apply() = black
}

case class RGB(red: Double, green: Double, blue: Double) {

  override def toString = "{R: " + red + ", G: " + green + ", B: " + blue + "}"

//  def apply(i: Int): Double = {
//    i match {
//      case 0 => red
//      case 1 => green
//      case 2 => blue
//    }
//  }

  def unary_+ = this

  def unary_- = Vector3(-red, -green, -blue)

  def +(that: RGB) = RGB(this.red + that.red, this.green + that.green, this.blue + that.blue)

  def -(that: RGB) = RGB(this.red - that.red, this.green - that.green, this.blue - that.blue)

  def *(that: RGB) = RGB(this.red * that.red, this.green * that.green, this.blue * that.blue)

  def *(d: Double) = RGB(red * d, green * d, blue * d)

  def /(d: Double) = this * (1.0 / d)


  def lengthSquared = red * red + green * green + blue * blue

  def length = math.sqrt(lengthSquared)

  def normalise = this / length

  def hasNaNs = red.isNaN || green.isNaN || blue.isNaN

  def clamp = RGB(MathUtil.clamp(red), MathUtil.clamp(green), MathUtil.clamp(blue))

  def max = math.max(math.max(red, green), blue)
}

/**
  * SuperSamp is a 2x2 grid of colours, used for super-sampling
  * in order improve anti-aliasing.
  */
object SuperSamp {
  final val black = SuperSamp(RGB.black, RGB.black, RGB.black, RGB.black)
}

case class SuperSamp(c00: RGB, c10: RGB, c01: RGB, c11: RGB) {

  def apply(x: Int, y: Int): RGB =
    (x, y) match {
      case (0, 0) => c00
      case (0, 1) => c01
      case (1, 0) => c10
      case (1, 1) => c11
    }

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

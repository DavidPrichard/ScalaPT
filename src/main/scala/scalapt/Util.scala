package scalapt

import scala.math.{sqrt, pow}

// miscellaneous functions and constants.
object Util {


  def clamp(x: Double, low: Double = 0.0, high: Double = 1.0): Double = {
    if (x > high)
      high
    else if (x < low)
      low
    else
      x
  }


  def sqr(x: Double) = x * x

}

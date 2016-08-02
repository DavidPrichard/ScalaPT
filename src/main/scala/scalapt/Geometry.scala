package scalapt

/**
  * Axis
  */
object Axis extends Enumeration {
  type Type = Value
  val X, Y, Z = Value
}

/**
  * Ray
  */
class Ray(val r: Point3, v: Vector3) {

  val dir = v.normalize

  def travels(length: Double): Point3 = r + dir * length
}


/**
  * Point3
  */
object Point3 {
}

case class Point3(x: Double, y: Double, z: Double) {

  def apply(ax: Axis.Type): Double = ax match {
    case Axis.X => x
    case Axis.Y => y
    case Axis.Z => z
  }

  def unary_- = Vector3(-x, -y, -z)

  def +(that: Vector3): Point3 = Point3(x + that.x, y + that.y, z + that.z)

  def -(that: Vector3): Point3 = Point3(x - that.x, y - that.y, z - that.z)

  def *(s: Double): Point3 = Point3(x * s, y * s, z * s)

  def /(s: Double): Point3 = Point3(x / s, y / s, z / s)

  def -(that: Point3): Vector3 = Vector3(x - that.x, y - that.y, z - that.z)

  def length = math.sqrt(lengthSquared)

  def lengthSquared = x*x + y*y + z*z

  def hasNaNs = x.isNaN || y.isNaN || z.isNaN
}

/**
  * Vector3
  */
object Vector3 {

  object XUnit extends Vector3(1.0, 0.0, 0.0)
  object YUnit extends Vector3(0.0, 1.0, 0.0)
  object ZUnit extends Vector3(0.0, 0.0, 1.0)

//  def unit(ax: Axis.Type): Vector3 = ax match {
//    case Axis.X => XUnit
//    case Axis.Y => YUnit
//    case Axis.Z => ZUnit
//  }
}

case class Vector3(x: Double, y: Double, z: Double) {

  def apply(ax: Axis.Type): Double = ax match {
    case Axis.X => x
    case Axis.Y => y
    case Axis.Z => z
  }

  def unary_- = Vector3(-x, -y, -z)

  def +(that: Vector3): Vector3 = Vector3(x + that.x, y + that.y, z + that.z)

  def -(that: Vector3): Vector3 = Vector3(x - that.x, y - that.y, z - that.z)

  def *(n: Double): Vector3 = Vector3(x * n, y * n, z * n)

  def /(n: Double): Vector3 = Vector3(x / n, y / n, z / n)

  def ∙(that: Vector3): Double = x * that.x + y * that.y + z * that.z

  def ×(that: Vector3): Vector3 =
        Vector3(
            y * that.z - z * that.y,
            z * that.x - x * that.z,
            x * that.y - y * that.x
        )

  def length: Double = math.sqrt(lengthSquared)

  def lengthSquared: Double = x*x + y*y + z*z

  def normalize: Vector3 = this * (1.0 / length)

  def hasNaNs: Boolean = x.isNaN || y.isNaN || z.isNaN
}


package scalapt

/**
  * Axis
  */
object Axis extends Enumeration {
  type Type = Value
  val X, Y, Z = Value
}

/**
  * Ray; has to be a case class for automatic Circe json serialization,
  * but needs its dir parameter normalized wherever the constructor is called.
  * TODO FIX IT
  */
case class Ray(origin: Point3, dir: Vector3) {

  def travels(length: Double): Point3 = origin + dir * length
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

  def asVector = Vector3(x, y, z)
}

/**
  * Vector3
  */
object Vector3 {
  final val Zero  = Vector3(0.0, 0.0, 0.0)

  final val XUnit = Vector3(1.0, 0.0, 0.0)
  final val YUnit = Vector3(0.0, 1.0, 0.0)
  final val ZUnit = Vector3(0.0, 0.0, 1.0)

  def unit(ax:Axis.Type): Vector3 = ax match {
    case Axis.X => XUnit
    case Axis.Y => YUnit
    case Axis.Z => ZUnit
  }
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

  def *(s: Double): Vector3 = Vector3(x * s, y * s, z * s)

  def /(s: Double): Vector3 = Vector3(x / s, y / s, z / s)

  def ∙(that: Vector3): Double = x * that.x + y * that.y + z * that.z

  def ×(that: Vector3): Vector3 =
        Vector3(
            y * that.z - z * that.y,
            z * that.x - x * that.z,
            x * that.y - y * that.x
        )

  def length: Double = math.sqrt(lengthSquared)


  def lengthSquared: Double = x*x + y*y + z*z

  def normalise: Vector3 = this * (1.0 / length)

  def hasNaNs: Boolean = x.isNaN || y.isNaN || z.isNaN
}


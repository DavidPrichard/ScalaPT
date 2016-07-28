package scalapt

import Shape._
/**
  * Shape
  */
object Shape {
  /**
    * Epsilon value to avoid object self-intersection.
    */
  final val ε = 1e-4
}

trait Shape {

  val name: String // For debugging.

  val material: Material

  def intersect(ray: Ray): Option[Double]

  def normal(p: Point3): Vector3
}

/**
  * Sphere
  */
case class Sphere(
  name: String,
  material: Material,
  centre: Point3,
  radius: Double
                 ) extends Shape {

  override def intersect(ray: Ray): Option[Double] = {
    val e = ray.r - centre
    val f = ray.v ∙ e
    val dsquared = f*f - (e ∙ e) + radius*radius

    if (dsquared > 0.0) {
      val determinant = math.sqrt(dsquared)
      val t = -f - determinant
      if (t > ε)
        Some(t)
      else
        None
    }
    else
      None

  }

  def normal(p: Point3) = (p - centre).normalise

}

/**
  * An axis-aligned infinite plane.
  * Allows light through in one direction (controlled by posFacing)
  */
case class Plane(
    name: String,
    material: Material,
    side: Axis.Type,
    posFacing: Boolean,
    v: Double
) extends Shape {

  def intersect(ray: Ray): Option[Double] = {
    if ((math.abs(ray.v(side)) > Double.MinPositiveValue) && ((ray.r(side) > v) == posFacing)) {
      val t = (v - ray.r(side)) / ray.v(side)
      if (t > ε)
        Some(t)
      else
        None
    }
    else
      None
  }

  def normal(p: Point3) =
    side match {
      case Axis.X => Vector3.XUnit
      case Axis.Y => Vector3.YUnit
      case Axis.Z => Vector3.ZUnit
    }
}

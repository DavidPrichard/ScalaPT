package scalapt

import Shape._
import math._

/**
  * Shape
  */
object Shape {

  // Epsilon value to avoid object self-intersection.
  final val ε = 1e-6
}

trait Shape {

  val name: String // For debugging.
  val mat: Material

  def intersect(ray: Ray): Option[(Shape, Double)]
  def normal(p: Point3): Vector3

  def deflect(ray: Ray, point: Point3, rand1: Double, rand2: Double): Ray = {

    val n = normal(point)
    val orientedN = if ((n ∙ ray.dir) < 0.0) n else -n // (n ∙ ray.dir) is always negative

    new Ray(point, mat.deflect(ray.dir, n, orientedN, rand1, rand2))
  }
}

/**
  * Sphere
  */
case class Sphere(
    name: String,
    mat: Material,
    center: Point3,
    radius: Double
) extends Shape {

  def intersect(ray: Ray): Option[(Shape, Double)] = {
    val e = ray.r - center
    val f = ray.dir ∙ e
    val d2 = f*f - (e∙e) + radius*radius // = discriminant squared

    if (d2 > 0.0) {
      val det = sqrt(d2)
      val t = -f - det
      if (t > ε)
        Some((this, t))
      else {
        val t = -f + det
        if (t > ε)
          Some((this, t))
        else
          None
      }
    }
    else
      None

  }

  def normal(p: Point3) = (p - center).normalize

}

/**
  * An axis-aligned infinite plane.
  * Allows light through in one direction, determined by posFacing.
  */
case class Plane(
    name: String,
    mat: Material,
    side: Axis.Type,
    posFacing: Boolean,
    v: Double
) extends Shape {

  def intersect(ray: Ray): Option[(Shape, Double)] = {
    if ((abs(ray.dir(side)) > Double.MinPositiveValue) && ((ray.r(side) > v) == posFacing)) {
      val t = (v - ray.r(side)) / ray.dir(side)
      if (t > ε)
        Some((this, t))
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

package scalapt

import Util._
import math._

/**
  * Material
  */
object Material {

  import RGB._

  def diffuse(color: RGB)    = new Diffuse(color, Black)
  def emissive(color: RGB)   = new Diffuse(Black, color)
  def refractive(color: RGB) = new Refractive(color, Black)
  def reflective(color: RGB) = new Reflective(color, Black)
}

trait Material {

  val color: RGB // this should perhaps be separate from material; or rather, a material should be a brdf and a color.
  val emission: RGB

  // ideal specular reflection
  def specReflect(in: Vector3, n: Vector3): Vector3 = in - n * 2 * (n ∙ in)

  // redirect a ray of light according to the properties of the material, including reflection and refraction
  def deflect(in: Vector3, normal: Vector3, orientedN: Vector3, rand1: Double, rand2: Double): Vector3
}

/**
  * Diffuse
  */
case class Diffuse(color: RGB, emission: RGB) extends Material {

  // diffuse reflection; odds of traveling in any direction in the hemisphere roughly equal
  def deflect(in: Vector3, normal: Vector3, w: Vector3, rand1: Double, rand2: Double): Vector3 = {

    val r1 = 2.0 * Pi * rand1 // random angle from 0 to 2pi (tau!)
    val r2s = sqrt(rand2) // random distance from 0 to radius (combined with prev, random point on circle)

    val u = ((if (abs(w.x) > 0.1) Vector3.YUnit else Vector3.XUnit) × w).normalize
    val v = w × u // w,u,v forms an orthonormal coordinate frame

    // random reflection direction
      u * cos(r1) * r2s +
      v * sin(r1) * r2s +
      w * sqrt(1.0 - rand2)
  }
}

/**
  * Refractive
  */
case class Refractive(color: RGB, emission: RGB) extends Material {

  // ideal refraction
  def deflect(in: Vector3, n: Vector3, orientedN: Vector3, rand1: Double, rand2: Double): Vector3 = {

    val into = (n ∙ orientedN) > 0.0

    val nt = 1.5
    val nnt = if (into) 1.0 / nt else nt
    val ddn = in ∙ orientedN
    val cos2t = 1.0 - nnt * nnt * (1.0 - ddn * ddn)

    if (cos2t < 0.0)
      specReflect(in, n) // Total internal reflection
    else {
      val sign = if (into) 1.0 else -1.0
      val tdir = (in * nnt - n * (sign * (ddn * nnt + sqrt(cos2t)))).normalize
      val r0 = sqr(nt - 1.0) / sqr(nt + 1.0)
      val c = 1.0 - (if (into) -ddn else tdir ∙ n)
      val re = r0 + (1.0 - r0) * c * c * c * c * c

      if (rand1 < (0.25 + re / 2.0))
        specReflect(in, n)
      else
        tdir

//      previous implementation. importance sampling!
//      rnd = RNG.nextDouble
//      if (rnd < q)
//        rt.radiance(reflRay, depth) * (re / q))
//      else
//        rt.radiance(Ray(p, tdir), depth) * ((1.0 - re) / (1.0 - q)))

    }
  }
}

/**
  * Reflective
  */
case class Reflective(color: RGB, emission: RGB) extends Material {

  // ideal specular reflection
  def deflect(in: Vector3, n: Vector3, orientedN: Vector3, rand1: Double, rand2: Double): Vector3 = specReflect(in, n)
}

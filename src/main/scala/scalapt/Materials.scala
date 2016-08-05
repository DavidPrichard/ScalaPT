package scalapt

import Util.sqr
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

    val n1 = 1.5 // ~refractive index of glass
    val n2 = 1.0 // ~refractive index of air
    val into = (n ∙ orientedN) > 0.0 // if ray is exiting or entering.
    val nnt = if (into) n2/n1 else n1/n2
    val ddn = in ∙ orientedN

    val cos2t = 1.0 - sqr(nnt) * (1.0 - sqr(ddn))
    if (cos2t < 0.0) {
      println("FUCK YES")
      specReflect(in, n) // total internal reflection
    }
    else {
      val sign = if (into) 1.0 else -1.0
      val refracted = (in * nnt - n * (sign * (ddn * nnt + sqrt(cos2t)))).normalize // refracted ray
      val cosθ = if (into) -ddn else refracted ∙ n

      // Schlick's Approximation
      val r0 = sqr((n1 - n2) / (n1 + n2)) // reflectance at normal (head-on) // = 0.04 for glass&air
      val re = r0 + (1.0 - r0) * pow(1.0 - cosθ, 5) // fresnel reflectance

      if (rand1 < re) specReflect(in, n) else refracted
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

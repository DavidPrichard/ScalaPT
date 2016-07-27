package scalapt

import Util._
import math._

/**
  * Material
  */
object Material {

  import RGB._

  def diffuse(colour: RGB)    = new Diffuse(colour, Black)
  def emissive(colour: RGB)   = new Diffuse(Black, colour)
  def refractive(colour: RGB) = new Refractive(colour, Black)
  def reflective(colour: RGB) = new Reflective(colour, Black)
}

trait Material {

  val colour: RGB
  val emission: RGB

  def radiance(
                rdr: RayTracer,
                ray: Ray,
                depth: Int,
                p: Point3,
                n: Vector3,
                nl: Vector3
              ): RNG.Type[RGB]
}

/**
  * Diffuse
  */
case class Diffuse(colour: RGB, emission: RGB) extends Material {

  override def radiance(
        rdr: RayTracer,
        ray: Ray,
        depth: Int,
        p: Point3,
        n: Vector3,
        w: Vector3 // formerly nl
    ): RNG.Type[RGB] = {

    RNG.nextDouble.flatMap(d1 => {
      val r1 = 2.0 * Pi * d1
      RNG.nextDouble.flatMap(r2 => {
        val r2s = sqrt(r2)
        val u = (if (abs(w.x) > 0.1) Vector3.YUnit else Vector3.XUnit) × w.normalise
        val v = w × u
        val d =
          u * math.cos(r1) * r2s +
          v * math.sin(r1) * r2s +
          w * math.sqrt(1.0 - r2)

        rdr.radiance(Ray(p, d.normalise), depth)
      })
    })
  }
}

/**
  * Refractive
  */
case class Refractive(colour: RGB, emission: RGB) extends Material {

  override def radiance(
        rt: RayTracer,
        ray: Ray,
        depth: Int,
        p: Point3,
        n: Vector3,
        nl: Vector3
    ): RNG.Type[RGB] = {

    val nt = 1.5
    val reflRay = Ray(p, (ray.dir - n * 2.0 * (n ∙ ray.dir)).normalise)
    val into = (n ∙ nl) > 0.0
    val nnt = if (into) 1.0 / nt else nt
    val ddn = ray.dir ∙ nl
    val cos2t = 1.0 - nnt * nnt * (1.0 - ddn * ddn)

    if (cos2t < 0.0)
      rt.radiance(reflRay, depth) // Total internal reflection
    else {
      val sign = if (into) 1.0 else -1.0
      val tdir = (ray.dir * nnt - n * (sign * (ddn * nnt + sqrt(cos2t)))).normalise
      val r0 = sqr(nt - 1.0) / sqr(nt + 1.0)
      val c = 1.0 - (if (into) -ddn else tdir ∙ n)
      val re = r0 + (1.0 - r0) * c * c * c * c * c

      val q = 0.25 + re / 2.0 // odds of being

      RNG.nextDouble.flatMap(rnd => {
        if (rnd < q)
          rt.radiance(reflRay, depth).map(rad => rad * (re / q))
        else
          rt.radiance(Ray(p, tdir.normalise), depth).map(rad => rad * ((1.0 - re) / (1.0 - q)))

      })
    }
  }
}

/**
  * Reflective
  */
case class Reflective(colour: RGB, emission: RGB) extends Material {
  override def radiance(
        rdr: RayTracer,
        ray: Ray,
        depth: Int,
        p: Point3,
        n: Vector3,
        nl: Vector3
    ): RNG.Type[RGB] = {

    val d = ray.dir - n * 2 * (n ∙ ray.dir)
    rdr.radiance(Ray(p, d.normalise), depth)
  }
}

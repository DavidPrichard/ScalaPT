package scalapt

import java.util.concurrent.TimeUnit

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object Concurrent {

  implicit final val ExCtx = ExecutionContext.fromExecutor(null)

  final val processorCount = Runtime.getRuntime.availableProcessors

  def For(range: Range)(doStuff: Int => Unit) = {
    val wait = Duration.create(10, TimeUnit.DAYS)

    range
      .sortBy(_ % processorCount)
      .grouped(range.size / processorCount)
      .toVector
      .map(subRange => Future(subRange.foreach(doStuff)))
      .foreach(f => Await.result(f, wait))
  }
}

/**
  * Pseudo-Monte-Carlo path tracing renderer.
  *
  * The RNG must produce uncorrelated, uniformly distributed numbers in [0,1]
  */
class RayTracer(val width: Int, val height: Int, val scene: Scene) {

  private val cx = Vector3(width * scene.camera.fov / height, 0.0, 0.0)
  private val cy = cx × scene.camera.dir * scene.camera.fov

  println("camera dir = " + scene.camera.dir)
  println("camera fov = " + scene.camera.fov)
  println(s"cx = $cx")
  println(s"cy = $cy")
  println("camray for (0,0) =" + camRay(0,0))
  println("camray for (1,1) =" + camRay(1,1))
  println("camray for (128,128) =" + camRay(128,128))


  // sample the radiance of light from a particular pixel at (x,y)
  def sample(x: Int, y: Int, rng: XorShift): RGB = {

      val subx = x + rng.next0to1
      val suby = y + rng.next0to1
      val dir = scene.camera.dir + camRay(subx, suby)
      //println(s"Ray direction component: $dir")
      radiance(new Ray(scene.camera.pos, dir), rng)
  }

  // tail-recursive version of ScalaPT's radiance function.
  // russian roulette's max could be removed, now that there is no danger of blowing the stack
  // russian roulette now takes into account the normalized previous color * current color,
  // so it should terminate more aggressively now.
  // Further advantages:
  // the output can be altered in interesting/valuable ways by passing different initial arguments
  // z.B. the attenuation, if it is set to RGB(1.0, 0.0, 0.0) will return on the red value detected in
  // the scene, but the russian roulette termination will also take this into account, as the
  // attenuation for green and blue will always be 0.
  @tailrec
  final def radiance(ray: Ray,
                     rng: XorShift,
                     accumulatedLight: RGB = RGB.Black,
                     attenuation: RGB = RGB.White
                    ): RGB = {

    scene.intersect(ray) match {
      case None => accumulatedLight
      case Some((obj, hitpoint)) =>

        val color  = obj.mat.color * attenuation // fraction of light unabsorbed by this and previous materials

        // Russian roulette termination; converts intensity of light to odds of continuing recursion.
        // that is: if the unabsorbed light is 0.70, that is normalized to 1.0,
        // but the light is given only a 70% chance to continue recursion.
        if (rng.next0to1 > color.max)
          accumulatedLight + (obj.mat.emission * attenuation)
        else
          radiance(
            obj.deflect(ray, hitpoint, rng.next0to1, rng.next0to1),
            rng,
            accumulatedLight + (obj.mat.emission * attenuation),
            color/color.max)
    }
  }

  def camRay(x: Double, y: Double): Vector3 = cx * (x/width - 0.5) + cy * (y/height - 0.5)


  private def tent(x: Double): Double = {
    if (x < 0.5)
      Math.sqrt(x * 2.0) - 1.0
    else
      1.0 - Math.sqrt(2.0 - x * 2.0)
  }

}
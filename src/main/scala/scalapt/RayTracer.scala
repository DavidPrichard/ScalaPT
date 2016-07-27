package scalapt

import java.util.concurrent.TimeUnit

import cats.data.State

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
  * Monte-Carlo path tracing renderer.
  */
class RayTracer(val width: Int, val height: Int, val scene: Scene) {

  private val cx = Vector3(width * scene.camera.fov / height, 0.0, 0.0)
  private val cy = cx.cross(scene.camera.ray.dir).normalise * scene.camera.fov

  def camRay(x: Double, y: Double): Vector3 =
    cx * (x / width - 0.5) + cy * (y / height - 0.5)

  def render(x: Int, y: Int): RNG.Type[SuperSamp] = {

    def subPixelRad(cx: Double, cy: Double): RNG.Type[RGB] = {
      RNG.nextDouble.flatMap(d => {
        val sx = x + (0.5 + cx + tent(d)) * 0.5
        RNG.nextDouble.flatMap(d2 => {
          val sy = y + (0.5 + cy + tent(d2)) * 0.5
          val dir = scene.camera.ray.dir + camRay(sx, sy)
          val ray = Ray(scene.camera.ray.origin, dir.normalise)
          radiance(ray, 0)
        })
      })
    }

    for {
      aa <- subPixelRad(0, 0)
      ba <- subPixelRad(1, 0)
      ab <- subPixelRad(0, 1)
      bb <- subPixelRad(1, 1)
    } yield SuperSamp(aa, ba, ab, bb)
  }

  def radiance(ray: Ray, depth: Int): RNG.Type[RGB] = {
    scene.intersect(ray) match {
      case None => State.pure(RGB.Black)
      case Some((hitObject, hitPoint)) =>
        val n = hitObject.normal(hitPoint)
        val nl =
          if (n.dot(ray.dir) < 0)
            n
          else
            -n


        val refl: RNG.Type[RGB] = {
          val colour = hitObject.material.colour

          if (depth > 5) {
            // Modified Russian roulette.
            val max = colour.max * Util.sqr(1.0 - depth / 200) // max to avoid stack overflow.
            RNG.nextDouble.flatMap(rnd => {
              if (rnd >= max)
                State.pure(RGB.Black)
              else 
                hitObject.material.radiance(this, ray, depth + 1, hitPoint, n, nl).map(_ * colour / max)
            })
          } else {
            hitObject.material.radiance(this, ray, depth + 1, hitPoint, n, nl).map(_ * colour)
          }
        }
        refl.map(_ + hitObject.material.emission)
    }
  }

  private def tent(x: Double): Double = {
    if (x < 0.5)
      Math.sqrt(x * 2.0) - 1.0
    else
      1.0 - Math.sqrt(2.0 - x * 2.0)
  }

}
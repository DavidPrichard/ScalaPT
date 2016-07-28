package scalapt

import java.util.concurrent.TimeUnit

import cats.data.State

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

import Util._
import scalapt.RNG.Type

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
  */
class RayTracer(val width: Int, val height: Int, val scene: Scene) {

  private val cx = Vector3(width * scene.camera.fov / height, 0.0, 0.0)
  private val cy = cx × scene.camera.dir * scene.camera.fov

  // sample the radiance of light coming from a particular pixel at (x,y)
  def render(x: Int, y: Int): RNG.Type[RGB] = {

    def subPixelRad(cx: Double, cy: Double): RNG.Type[RGB] = {
      RNG.nextDouble.flatMap(d => {
        val sx = x + (0.5 + cx + tent(d)) * 0.5
        RNG.nextDouble.flatMap(d2 => {
          val sy = y + (0.5 + cy + tent(d2)) * 0.5
          val dir = scene.camera.dir + camRay(sx, sy)
          val ray = Ray(scene.camera.pos, dir)
          radiance(ray, 0)
        })
      })
    }

    for {
      aa <- subPixelRad(0, 0)
      ba <- subPixelRad(1, 0)
      ab <- subPixelRad(0, 1)
      bb <- subPixelRad(1, 1)
    } yield (aa + ba + ab + bb) / 4
  }

  //@tailrec
  final def radiance(ray: Ray, depth: Int): RNG.Type[RGB] = {

    scene.intersect(ray) match {
      case None => State.pure(RGB.Black)
      case Some((hitObject, hitPoint)) =>

        val normal = hitObject.normal(hitPoint)
        val color = hitObject.material.colour

        val nl =
          if ((normal ∙ ray.v) < 0)
            normal
          else
            -normal


        val refl: RNG.Type[RGB] = {

          if (depth > 5) {
            // Modified Russian roulette.
            val max = color.max * sqr(1.0 - depth / 200) // max to avoid stack overflow.
            RNG.nextDouble.flatMap(rnd => {
              if (rnd >= max)
                State.pure(RGB.Black)
              else 
                hitObject.material.radiance(this, ray, depth + 1, hitPoint, normal, nl).map(_ * color / max)
            })
          }
          else {
            hitObject.material.radiance(this, ray, depth + 1, hitPoint, normal, nl).map(_ * color)
          }
        }

        refl.map(x => x + hitObject.material.emission)
    }
  }

  def camRay(x: Double, y: Double): Vector3 =
    cx * (x / width - 0.5) +
      cy * (y / height - 0.5)


  private def tent(x: Double): Double = {
    if (x < 0.5)
      Math.sqrt(x * 2.0) - 1.0
    else
      1.0 - Math.sqrt(2.0 - x * 2.0)
  }

}
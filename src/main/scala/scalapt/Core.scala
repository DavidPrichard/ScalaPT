package scalapt

object MathUtil {

  import scala.math.{sqrt, pow}

  /**
    * Tent function
    */
  def tent(x: Double): Double = {
    if (x < 0.5)
      sqrt(x * 2.0) - 1.0
    else
      1.0 - sqrt(2.0 - x * 2.0)
  }

  /**
    * Clamp a value to a range.
    */
  def clamp(x: Double, low: Double = 0.0, high: Double = 1.0): Double = {
    if (x > high)
      high
    else if (x < low)
      low
    else
      x
  }

  def sqr(x: Double) = {
    x * x
  }

  final val GAMMA = 2.2

  def gammaCorrection(x: Double) = pow(clamp(x), 1.0 / GAMMA)
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
  * Material
  */
object Material {
  def diffuse(r: Double, g: Double, b: Double) = new Diffuse(RGB(r, g, b), RGB.black)

  def diffuse(colour: RGB) = new Diffuse(colour, RGB.black)

  def emissive(r: Double, g: Double, b: Double) = new Diffuse(RGB.black, RGB(r, g, b), true)

  def emissive(colour: RGB) = new Diffuse(RGB.black, colour, true)

  def refractive(r: Double, g: Double, b: Double) = new Refractive(RGB(r, g, b), RGB.black)

  def refractive(colour: RGB) = new Refractive(colour, RGB.black)

  def reflective(r: Double, g: Double, b: Double) = new Reflective(RGB(r, g, b), RGB.black)

  def reflective(colour: RGB) = new Reflective(colour, RGB.black)
}

trait Material {

  val colour: RGB
  val emColour: RGB

  def emission: RGB = emColour

  def radiance(
    rdr: Renderer,
    ray: Ray,
    depth: Int,
    p: Point3,
    n: Vector3,
    nl: Vector3
    ): RNG.Type[RGB]
}

/**
 * Camera
 */
case class Camera(ray: Ray, fov: Double)

/**
  * Shape
  */
object Shape {
    /**
      * Epsilon value to avoid object self-intersection.
      */
    final val epsilon = 1e-4
}

trait Shape {

  val name: String // For debugging.

  val material: Material

  def intersect(ray: Ray, eps: Double): Option[Double]

  def intersect(ray: Ray): Option[Double] = intersect(ray, Shape.epsilon)

  def normal(p: Point3): Vector3
}

/**
  * Scene
  */
object Scene {
  val distance = Ordering.by((_: (Shape, Double))._2)
}

case class Scene(camera: Camera, shapes: List[Shape]) {
  /**
    * Find the closest shape intersected by the ray.
    */
  def intersect(ray: Ray): Option[(Shape, Point3)] = {
    shapes
      .flatMap(obj => obj.intersect(ray).map(t => (obj, t)))
      .reduceOption(Scene.distance.min)
      .map({ case (obj, t) => (obj, ray.travels(t)) })
  }
}

/**
  * Renderer
  */
object Renderer {
  // Need a maximum to avoid stack overflow.
  final val MaxDepth = 200
}

trait Renderer {
  val width: Int
  val height: Int
  val scene: Scene

  private val cx = Vector3(width * scene.camera.fov / height, 0.0, 0.0)
  private val cy = cx.cross(scene.camera.ray.dir).normalise * scene.camera.fov

  def camRay(xs: Double, ys: Double): Vector3 =
    cx * (xs / width - 0.5) +
      cy * (ys / height - 0.5)

  def render(x: Int, y: Int): RNG.Type[SuperSamp] = {
    def subPixelRad(cx: Double, cy: Double): RNG.Type[RGB] = {
      RNG.nextDouble.flatMap(d1 => {
        RNG.nextDouble.flatMap(d2 => {
          val dx = MathUtil.tent(d1)
          val dy = MathUtil.tent(d2)
          val sx = x + (0.5 + cx + dx) * 0.5
          val sy = y + (0.5 + cy + dy) * 0.5
          val dir = scene.camera.ray.dir + camRay(sx, sy)
          val origin = scene.camera.ray.origin
          val ray = Ray(origin, dir.normalise)
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

  def radiance(ray: Ray, depth: Int): RNG.Type[RGB]
}

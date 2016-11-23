package scalapt

/**
  * Camera
  */
case class Camera(pos: Point3, override val dir: Vector3, fov: Double)
  extends Ray(pos, dir)

/**
  * Scene
  */
object Scene {
  val distance: Ordering[(Shape, Double)] = Ordering.by((_: (Shape, Double))._2)
}

case class Scene(camera: Camera, shapes: Vector[Shape]) {

  // Find the closest shape intersected by the ray.
  def intersect(ray: Ray): Option[(Shape, Point3)] = {
    shapes
      .flatMap(_.intersect(ray))
      .reduceOption(Scene.distance.min) // pick closest one
      .map{ case (obj, t) => (obj, ray.travels(t)) }
  }
}
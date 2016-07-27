package scalapt

/**
  * Camera
  */
case class Camera(ray: Ray, fov: Double)

/**
  * Scene
  */
object Scene {
  val distance = Ordering.by((_: (Shape, Double))._2)
}

case class Scene(camera: Camera, shapes: Vector[Shape]) {

  // Find the closest shape intersected by the ray.
  def intersect(ray: Ray): Option[(Shape, Point3)] = {
    shapes
      .flatMap(obj => obj.intersect(ray).map(t => (obj, t)))
      .reduceOption(Scene.distance.min)
      .map({ case (obj, t) => (obj, ray.travels(t)) })
  }
}

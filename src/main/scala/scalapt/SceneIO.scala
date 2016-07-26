package scalapt

import java.nio.file.{Files, Paths}
import java.util.NoSuchElementException

import cats.data.Xor
import io.circe._
import io.circe.Decoder
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._


object SceneIO {

  import Codecs._


  def load(fileName: String): Scene = {

    val encoded = new String(Files.readAllBytes(Paths.get(fileName)))

    decode[Scene](encoded).valueOr(err => throw err)
  }

  def save(scene: Scene, fileName: String) = {

    val encoded = scene.asJson.spaces4

    Files.write(Paths.get(fileName), encoded.getBytes)
  }
}


object Codecs {

  final val typeString = "type"
  
  def withType(json: Json, name: String): Json = json.mapObject(jo => jo.+:(typeString, name.asJson))

  implicit val decodeAxis: Decoder[Axis.Type] =
    Decoder.instance(c => c.focus.asString match {
        case Some(s) =>
          try Xor.right(Axis.withName(s))
          catch {
            case ex: NoSuchElementException => Xor.left(DecodingFailure(ex.getMessage, c.history))
          }
        case None =>
          Xor.left(DecodingFailure("String", c.history))
      }
    )

  implicit val encodeAxis: Encoder[Axis.Type] = Encoder.instance(_.toString.asJson)

  implicit val decodeMaterial: Decoder[Material] =
    Decoder.instance(c =>
      c.downField(typeString).as[String].flatMap {
        case "Diffuse"    => c.as[Diffuse]
        case "Reflective" => c.as[Reflective]
        case "Refractive" => c.as[Refractive]
      }
    )

  implicit val encodeMaterial: Encoder[Material] =
    Encoder.instance {
      case (d: Diffuse)    => withType(d.asJson, "Diffuse")
      case (r: Reflective) => withType(r.asJson, "Reflective")
      case (r: Refractive) => withType(r.asJson, "Refractive")
    }

  implicit val decodeShape: Decoder[Shape] =
    Decoder.instance(c =>
      c.downField(typeString).as[String].flatMap {
        case "Plane"  => c.as[Plane]
        case "Sphere" => c.as[Sphere]
      }
    )

  implicit val encodeShape: Encoder[Shape] =
    Encoder.instance {
      case (p: Plane)  => withType(p.asJson, "Plane")
      case (s: Sphere) => withType(s.asJson, "Sphere")
    }

}
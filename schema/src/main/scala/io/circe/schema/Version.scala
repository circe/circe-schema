package io.circe.schema

import cats.kernel.Order
import io.circe.Decoder
import java.net.URI

sealed trait Version {
  def uri: URI
}

object Version {
  case object Draft4 extends Version {
    val uri: URI = new URI("http://json-schema.org/draft-04/schema#")
  }

  case object Draft6 extends Version {
    val uri: URI = new URI("http://json-schema.org/draft-06/schema#")
  }

  case object Draft7 extends Version {
    val uri: URI = new URI("http://json-schema.org/draft-07/schema#")
  }

  case object Draft201909 extends Version {
    val uri: URI = new URI("http://json-schema.org/draft-2019-09/schema#")
  }

  implicit val versionOrder: Order[Version] = Order.by(_.uri.toString)
  implicit val versionOrdering: Ordering[Version] = versionOrder.toOrdering

  private val byURI: Map[String, Version] =
    List(Draft4, Draft6, Draft7, Draft201909).map(version => (version.uri.toString, version)).toMap

  implicit val decodeVersion: Decoder[Version] = Decoder[String].emap(value => byURI.get(value).toRight("Version"))
}

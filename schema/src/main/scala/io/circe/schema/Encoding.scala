package io.circe.schema

import io.circe.{Decoder, Encoder}

sealed trait Encoding {
  def name: String
}

object Encoding {
  case object SevenBit extends Encoding {
    def name: String = "7bit"
  }

  case object EightBit extends Encoding {
    def name: String = "8bit"
  }

  case object Binary extends Encoding {
    def name: String = "binary"
  }

  case object QuotedPrintable extends Encoding {
    def name: String = "quoted-printable"
  }

  case object Base64 extends Encoding {
    def name: String = "base64"
  }

  private val byName: Map[String, Encoding] =
    List(SevenBit, EightBit, Binary, QuotedPrintable, Base64).map(tpe => (tpe.name, tpe)).toMap

  implicit val decodeEncoding: Decoder[Encoding] =
    Decoder[String].emap(value => byName.get(value).toRight("Encoding"))
  implicit val encodeEncoding: Encoder[Encoding] = Encoder[String].contramap(_.name)
}

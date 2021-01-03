package io.circe.schema

import cats.syntax.apply._
import io.circe.{Decoder, Json, JsonObject}

case class Metadata(
  id: Option[String],
  version: Option[Version],
  title: Option[String],
  description: Option[String],
  default: Option[Json],
  examples: Option[List[Json]]
)

object Metadata {
  val empty: Metadata = Metadata(None, None, None, None, None, None)

  private val idDecoderDraft4: Decoder[Option[String]] = Decoder[Option[String]].at("id")
  private val idDecoderOther: Decoder[Option[String]] = Decoder[Option[String]].at("$id")

  implicit val decodeMetadata: Decoder[Metadata] = (
    Decoder[JsonObject],
    Decoder[Option[Version]].at("$schema"),
    Decoder[Option[String]].at("title"),
    Decoder[Option[String]].at("description"),
    Decoder[Option[Json]].at("default"),
    Decoder[Option[List[Json]]].at("examples")
  ).mapN((_, version, title, description, default, examples) =>
    Metadata(None, version, title, description, default, examples)
  ).flatMap { metadata =>
    val idDecoder = if (metadata.version.contains(Version.Draft4)) {
      idDecoderDraft4
    } else {
      idDecoderOther
    }

    idDecoder.map(_.fold(metadata)(id => metadata.copy(id = Some(id))))
  }
}

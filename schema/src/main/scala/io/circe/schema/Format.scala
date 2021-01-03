package io.circe.schema

import io.circe.{Decoder, Encoder}

sealed trait Format {
  def name: String
  def since: Version
}

object Format {
  case object DateTime extends Format {
    def name: String = "date-time"
    def since: Version = Version.Draft4
  }

  case object Date extends Format {
    def name: String = "date"
    def since: Version = Version.Draft7
  }

  case object Time extends Format {
    def name: String = "time"
    def since: Version = Version.Draft7
  }

  case object Email extends Format {
    def name: String = "email"
    def since: Version = Version.Draft4
  }

  case object EmailIdn extends Format {
    def name: String = "idn-email"
    def since: Version = Version.Draft7
  }

  case object Hostname extends Format {
    def name: String = "hostname"
    def since: Version = Version.Draft4
  }

  case object HostnameIdn extends Format {
    def name: String = "idn-hostname"
    def since: Version = Version.Draft7
  }

  case object Ipv4 extends Format {
    def name: String = "ipv4"
    def since: Version = Version.Draft4
  }

  case object Ipv6 extends Format {
    def name: String = "ipv6"
    def since: Version = Version.Draft4
  }

  case object Uri extends Format {
    def name: String = "uri"
    def since: Version = Version.Draft4
  }

  case object UriReference extends Format {
    def name: String = "uri-reference"
    def since: Version = Version.Draft6
  }

  case object Iri extends Format {
    def name: String = "iri"
    def since: Version = Version.Draft7
  }

  case object IriReference extends Format {
    def name: String = "iri-reference"
    def since: Version = Version.Draft7
  }

  case object UriTemplate extends Format {
    def name: String = "uri-template"
    def since: Version = Version.Draft6
  }

  case object JsonPointer extends Format {
    def name: String = "json-pointer"
    def since: Version = Version.Draft6
  }

  case object RelativeJsonPointer extends Format {
    def name: String = "relative-json-pointer"
    def since: Version = Version.Draft7
  }

  case object Regex extends Format {
    def name: String = "regex"
    def since: Version = Version.Draft7
  }

  private val byName: Map[String, Format] =
    List(
      DateTime,
      Date,
      Time,
      Email,
      EmailIdn,
      Hostname,
      HostnameIdn,
      Ipv4,
      Ipv6,
      Uri,
      UriReference,
      Iri,
      IriReference,
      UriTemplate,
      JsonPointer,
      RelativeJsonPointer,
      Regex
    ).map(tpe => (tpe.name, tpe)).toMap

  implicit val decodeFormat: Decoder[Format] = Decoder[String].emap(value => byName.get(value).toRight("Format"))
  implicit val encodeFormat: Encoder[Format] = Encoder[String].contramap(_.name)
}

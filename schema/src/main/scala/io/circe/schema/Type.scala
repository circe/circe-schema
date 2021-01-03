package io.circe.schema

import io.circe.{Decoder, Encoder}

sealed trait Type {
  def name: String
}

object Type {
  case object String extends Type {
    def name: java.lang.String = "string"
  }

  case object Integer extends Type {
    def name: java.lang.String = "integer"
  }

  case object Number extends Type {
    def name: java.lang.String = "number"
  }

  case object Object extends Type {
    def name: java.lang.String = "object"
  }

  case object Array extends Type {
    def name: java.lang.String = "array"
  }

  case object Boolean extends Type {
    def name: java.lang.String = "boolean"
  }

  case object Null extends Type {
    def name: java.lang.String = "null"
  }

  private val byName: Map[java.lang.String, Type] =
    List(String, Integer, Number, Object, Array, Boolean, Null).map(tpe => (tpe.name, tpe)).toMap

  implicit val decodeType: Decoder[Type] = Decoder[java.lang.String].emap(value => byName.get(value).toRight("Type"))
  implicit val encodeType: Encoder[Type] = Encoder[java.lang.String].contramap(_.name)
}

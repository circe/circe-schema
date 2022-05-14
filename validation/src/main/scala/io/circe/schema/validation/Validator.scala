package io.circe.schema.validation

import io.circe.{HCursor, Json}

trait Validator extends HCursor => Vector[ValidationError] {
  final def isValid(value: Json): Boolean = apply(value.hcursor).isEmpty
}

object Validator {
  object AcceptAll extends Validator {
    def apply(c: HCursor): Vector[ValidationError] = Vector.empty
  }
  object RejectAll extends Validator {
    def apply(c: HCursor): Vector[ValidationError] = Vector(RejectAllError(c.history))
  }
  case class Combined(validators: Vector[Validator]) extends Validator {
    def apply(c: HCursor): Vector[ValidationError] = validators.flatMap(_(c))
  }
}

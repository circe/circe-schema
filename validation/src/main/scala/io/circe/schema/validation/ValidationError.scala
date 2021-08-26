package io.circe.schema.validation

import io.circe.{CursorOp, Json}
import io.circe.schema.{Schema, Type}

sealed abstract class ValidationError extends Exception {
  def history: List[CursorOp]

  final def withHistory(newHistory: List[CursorOp]): ValidationError = this match {
    case e @ EnumerationError(_, _)              => e.copy(history = newHistory)
    case e @ TypeError(_, _)                     => e.copy(history = newHistory)
    case e @ RejectAllError(_)                   => e.copy(history = newHistory)
    case e @ ArrayAdditionalItemError(_, _)      => e.copy(history = newHistory)
    case e @ ObjectRequiredPropertyError(_, _)   => e.copy(history = newHistory)
    case e @ ObjectMinPropertiesError(_, _, _)   => e.copy(history = newHistory)
    case e @ ObjectMaxPropertiesError(_, _, _)   => e.copy(history = newHistory)
    case e @ ObjectAdditionalPropertyError(_, _) => e.copy(history = newHistory)
    case e @ AnyOfError(_, _)                    => e.copy(history = newHistory)
    case e @ OneOfError(_, _)                    => e.copy(history = newHistory)
    case e @ NotError(_, _)                      => e.copy(history = newHistory)
    case e @ NumericConstraintError(_, _, _)     => e.copy(history = newHistory)
    case e @ StringConstraintError(_, _, _)      => e.copy(history = newHistory)
    case e @ ArrayConstraintError(_, _)          => e.copy(history = newHistory)
  }
}

case class EnumerationError(history: List[CursorOp], expected: List[Json]) extends ValidationError {
  final override def getMessage(): String =
    s"Expected one of [${expected.map(_.noSpaces).mkString(", ")}]"
}

case class TypeError(history: List[CursorOp], expected: Vector[Type]) extends ValidationError {
  final override def getMessage(): String = s"Expected type to be one of ${expected.mkString(", ")}"
}

case class RejectAllError(history: List[CursorOp]) extends ValidationError {
  final override def getMessage(): String = s"Schema rejects all values"
}

case class ArrayAdditionalItemError(history: List[CursorOp], index: Int) extends ValidationError {
  final override def getMessage(): String = s"Invalid additional array item at index ${index}"
}

case class ObjectRequiredPropertyError(history: List[CursorOp], name: String) extends ValidationError {
  final override def getMessage(): String = s"Expected object with property ${name}"
}

case class ObjectMinPropertiesError(history: List[CursorOp], length: Int, n: Int) extends ValidationError {
  final override def getMessage(): String = s"Expected object with >= ${n} properties, got ${length}"
}

case class ObjectMaxPropertiesError(history: List[CursorOp], length: Int, n: Int) extends ValidationError {
  final override def getMessage(): String = s"Expected object with <= ${n} properties, got ${length}"
}

case class ObjectAdditionalPropertyError(history: List[CursorOp], name: String) extends ValidationError {
  final override def getMessage(): String = s"Invalid additional object property ${name}"
}

case class AnyOfError(
  history: List[CursorOp],
  errors: Vector[(Schema.Resolved, Vector[ValidationError])]
) extends ValidationError {
  final override def getMessage(): String = "Expected at least one schema to match"
}

case class OneOfError(
  history: List[CursorOp],
  errors: Vector[(Schema.Resolved, Vector[ValidationError])]
) extends ValidationError {
  final override def getMessage(): String = "Expected exactly one schema to match"
}

case class NotError(history: List[CursorOp], schema: Schema.Resolved) extends ValidationError {
  final override def getMessage(): String = "Expected schema to fail"
}

case class NumericConstraintError(
  history: List[CursorOp],
  number: Option[BigDecimal],
  constraint: io.circe.schema.Constraint.NumericConstraint
) extends ValidationError {
  private[this] val asString: String =
    number.fold("a number that cannot be represented as a BigDecimal value")(_.toString)

  final override def getMessage(): String = constraint match {
    case io.circe.schema.Constraint.Minimum(target)          => s"Expected number >= ${target}, got ${asString}"
    case io.circe.schema.Constraint.Maximum(target)          => s"Expected number <= ${target}, got ${asString}"
    case io.circe.schema.Constraint.ExclusiveMinimum(target) => s"Expected number > ${target}, got ${asString}"
    case io.circe.schema.Constraint.ExclusiveMaximum(target) => s"Expected number < ${target}, got ${asString}"
    case io.circe.schema.Constraint.MultipleOf(target)       => s"Expected a multiple of ${target}, got ${asString}"
  }
}

case class StringConstraintError(
  history: List[CursorOp],
  value: String,
  constraint: io.circe.schema.Constraint.StringConstraint
) extends ValidationError {
  final override def getMessage(): String = constraint match {
    case io.circe.schema.Constraint.Pattern(regex)                 => s"Expected string matching ${regex}, got ${value}"
    case io.circe.schema.Constraint.Format(format)                 => s"Expected ${format.name} string, got ${value}"
    case io.circe.schema.Constraint.ContentMediaType(mediaType, _) => s"Expected ${mediaType} string, got ${value}"
    case io.circe.schema.Constraint.ContentEncoding(encoding)      => s"Expected ${encoding.name} string, got ${value}"
    case io.circe.schema.Constraint.MinLength(n) => s"Expected string with length >= ${n}, got ${value}"
    case io.circe.schema.Constraint.MaxLength(n) => s"Expected string with length <= ${n}, got ${value}"
  }
}

case class ArrayConstraintError(
  history: List[CursorOp],
  constraint: io.circe.schema.Constraint.ArrayConstraint[Schema.Resolved]
) extends ValidationError {
  final override def getMessage(): String = constraint match {
    case io.circe.schema.Constraint.UniqueItems(false) => "Unexpected error"
    case io.circe.schema.Constraint.UniqueItems(true)  => "Expected unique values in array"
    case io.circe.schema.Constraint.MinItems(n)        => s"Expected array with length >= ${n}"
    case io.circe.schema.Constraint.MaxItems(n)        => s"Expected array with length <= ${n}"
    case io.circe.schema.Constraint.Contains(schema)   => "Expected at least one array value to match schema"
  }
}

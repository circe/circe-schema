package io.circe.schema.validation

import cats.kernel.Eq
import io.circe.{HCursor, Json, JsonNumber, JsonObject}
import io.circe.schema.{Constraint, Dependency, Property, Schema, Type}
import io.circe.schema.validation.{FormatValidator => ValidationFormatValidator}
import java.util.regex.Pattern

trait Compiler {
  final def apply(schema: Schema.Resolved): Validator = schema match {
    case Schema.Constraints(_, constraints) =>
      if (constraints.size == 1) compile(constraints(0)) else Validator.Combined(constraints.map(compile))
    case Schema.AcceptAll       => Validator.AcceptAll
    case Schema.RejectAll       => Validator.RejectAll
    case Schema.Link(schema)    => apply(schema.value)
    case Schema.Ref(_, nothing) => nothing
  }

  def compile(constraint: Constraint[Schema.Resolved]): Validator
}

object Compiler {
  val default: Compiler = apply()
  def apply(
    config: Configuration = Configuration.default,
    formatValidator: ValidationFormatValidator = ValidationFormatValidator.default,
    mediaTypeValidator: MediaTypeValidator = MediaTypeValidator.default,
    encodingValidator: EncodingValidator = EncodingValidator.default
  )(implicit eqJson: Eq[Json]): Compiler =
    new Impl(config, formatValidator, mediaTypeValidator, encodingValidator, eqJson)

  private class Impl(
    config: Configuration,
    val formatValidator: ValidationFormatValidator,
    val mediaTypeValidator: MediaTypeValidator,
    val encodingValidator: EncodingValidator,
    val eqJson: Eq[Json]
  ) extends Compiler {
    def compile(constraint: Constraint[Schema.Resolved]): Validator = constraint match {
      case Constraint.Types(values)           => new TypesValidator(values)
      case Constraint.Enumeration(values)     => new EnumerationValidator(values, eqJson)
      case Constraint.AllOf(schemas)          => new AllOfValidator(schemas, this)
      case Constraint.AnyOf(schemas)          => new AnyOfValidator(schemas, this)
      case Constraint.OneOf(schemas)          => new OneOfValidator(schemas, this)
      case Constraint.Not(schema)             => new NotValidator(schema, this)
      case c @ Constraint.Minimum(_)          => new MinimumValidator(c)
      case c @ Constraint.Maximum(_)          => new MaximumValidator(c)
      case c @ Constraint.ExclusiveMinimum(_) => new ExclusiveMinimumValidator(c)
      case c @ Constraint.ExclusiveMaximum(_) => new ExclusiveMaximumValidator(c)
      case c @ Constraint.MultipleOf(_)       => new MultipleOfValidator(c, config.multipleOfBigDecimal)
      case c @ Constraint.Pattern(_)          => new PatternValidator(c)
      case c @ Constraint.Format(_)           => new FormatValidator(c, formatValidator)
      case c @ Constraint.ContentMediaType(_, _) =>
        new ContentMediaTypeValidator(c, mediaTypeValidator, encodingValidator)
      case c @ Constraint.ContentEncoding(_)              => new ContentEncodingValidator(c, encodingValidator)
      case c @ Constraint.MinLength(_)                    => new MinLengthValidator(c)
      case c @ Constraint.MaxLength(_)                    => new MaxLengthValidator(c)
      case Constraint.UniqueItems(false)                  => Validator.AcceptAll
      case c @ Constraint.UniqueItems(true)               => new UniqueItemsValidator(c, eqJson)
      case c @ Constraint.MinItems(_)                     => new MinItemsValidator(c)
      case c @ Constraint.MaxItems(_)                     => new MaxItemsValidator(c)
      case c @ Constraint.Contains(_)                     => new ContainsValidator(c, this)
      case Constraint.ItemsType(schema)                   => new ItemsTypeValidator(schema, 0, this)
      case Constraint.ItemsTuple(schemas)                 => new ItemsTupleValidator(schemas, this)
      case Constraint.AdditionalItems(schema, startIndex) => new ItemsTypeValidator(schema, startIndex, this)
      case Constraint.Properties(values)                  => new PropertiesValidator(values, this)
      case Constraint.Required(names)                     => new RequiredValidator(names)
      case Constraint.PropertyNames(schema)               => new PropertyNamesValidator(schema, this)
      case Constraint.MinProperties(limit)                => new MinPropertiesValidator(limit)
      case Constraint.MaxProperties(limit)                => new MaxPropertiesValidator(limit)
      case Constraint.PatternProperties(values)           => new PatternPropertiesValidator(values, this)
      case Constraint.AdditionalProperties(schema, knownNames, patterns) =>
        new AdditionalPropertiesValidator(schema, knownNames, patterns, this)
      case Constraint.Dependencies(values) => new DependenciesValidator(values, this)
      case Constraint.Conditional(ifSchema, thenSchema, elseSchema) =>
        new ConditionalValidator(ifSchema, thenSchema, elseSchema, this)
    }
  }

  private final class TypesValidator(values: Vector[Type]) extends Validator {
    private[this] val acceptArray: Boolean = values.contains(Type.Array)
    private[this] val acceptBoolean: Boolean = values.contains(Type.Boolean)
    private[this] val acceptInteger: Boolean = values.contains(Type.Integer)
    private[this] val acceptNull: Boolean = values.contains(Type.Null)
    private[this] val acceptNumber: Boolean = values.contains(Type.Number)
    private[this] val acceptObject: Boolean = values.contains(Type.Object)
    private[this] val acceptString: Boolean = values.contains(Type.String)

    private[this] val folder: Json.Folder[Boolean] = new Json.Folder[Boolean] {
      def onArray(value: Vector[Json]): Boolean = acceptArray
      def onBoolean(value: Boolean): Boolean = acceptBoolean
      def onNull: Boolean = acceptNull
      def onObject(value: JsonObject): Boolean = acceptObject
      def onString(value: String): Boolean = acceptString
      def onNumber(value: JsonNumber): Boolean = if (acceptNumber) {
        true
      } else if (acceptInteger) {
        value.toBigDecimal.exists(_.isWhole)
      } else {
        false
      }
    }

    def apply(c: HCursor): Vector[ValidationError] = if (c.value.foldWith(folder)) {
      Vector.empty
    } else {
      Vector(TypeError(c.history, values))
    }
  }

  private final class EnumerationValidator(values: Vector[Json], eqJson: Eq[Json]) extends Validator {
    def apply(c: HCursor): Vector[ValidationError] = if (values.exists(eqJson.eqv(c.value, _))) {
      Vector.empty
    } else {
      Vector(EnumerationError(c.history, values.toList))
    }
  }

  private final class AllOfValidator(schemas: Vector[Schema.Resolved], compiler: Compiler) extends Validator {
    private[this] val compiled: Vector[Validator] = schemas.map(compiler(_))

    def apply(c: HCursor): Vector[ValidationError] = compiled.flatMap(_(c))
  }

  private final class AnyOfValidator(schemas: Vector[Schema.Resolved], compiler: Compiler) extends Validator {
    private[this] val compiled: Vector[(Schema.Resolved, Validator)] = schemas.map(schema => (schema, compiler(schema)))

    def apply(c: HCursor): Vector[ValidationError] = {
      val results = compiled.map { case (schema, validator) =>
        (schema, validator(c))
      }

      if (results.exists(_._2.isEmpty)) {
        Vector.empty
      } else {
        Vector(AnyOfError(c.history, results))
      }
    }
  }

  private final class OneOfValidator(schemas: Vector[Schema.Resolved], compiler: Compiler) extends Validator {
    private[this] val compiled: Vector[(Schema.Resolved, Validator)] = schemas.map(schema => (schema, compiler(schema)))

    def apply(c: HCursor): Vector[ValidationError] = {
      val results = compiled.map { case (schema, validator) =>
        (schema, validator(c))
      }

      if (results.count(_._2.isEmpty) == 1) {
        Vector.empty
      } else {
        Vector(OneOfError(c.history, results))
      }
    }
  }

  private final class NotValidator(schema: Schema.Resolved, compiler: Compiler) extends Validator {
    private[this] val compiled: Validator = compiler(schema)

    def apply(c: HCursor): Vector[ValidationError] = if (compiled(c).nonEmpty) {
      Vector.empty
    } else {
      Vector(NotError(c.history, schema))
    }
  }

  private abstract class NumericConstraintValidator(constraint: Constraint.NumericConstraint) extends Validator {
    protected[this] def check(actualValue: BigDecimal, constraintValue: BigDecimal): Boolean

    final def apply(c: HCursor): Vector[ValidationError] = c.value.asNumber match {
      case Some(asNumber) =>
        asNumber.toBigDecimal match {
          case Some(asBigDecimal) =>
            if (check(asBigDecimal, constraint.value)) {
              Vector.empty
            } else {
              Vector(NumericConstraintError(c.history, Some(asBigDecimal), constraint))
            }
          case None => Vector(NumericConstraintError(c.history, None, constraint))
        }
      case None => Vector.empty
    }
  }

  private final class MinimumValidator(constraint: Constraint.NumericConstraint)
      extends NumericConstraintValidator(constraint) {
    protected[this] def check(actualValue: BigDecimal, constraintValue: BigDecimal): Boolean =
      actualValue >= constraintValue
  }

  private final class MaximumValidator(constraint: Constraint.NumericConstraint)
      extends NumericConstraintValidator(constraint) {
    protected[this] def check(actualValue: BigDecimal, constraintValue: BigDecimal): Boolean =
      actualValue <= constraintValue
  }

  private final class ExclusiveMinimumValidator(constraint: Constraint.NumericConstraint)
      extends NumericConstraintValidator(constraint) {
    protected[this] def check(actualValue: BigDecimal, constraintValue: BigDecimal): Boolean =
      actualValue > constraintValue
  }

  private final class ExclusiveMaximumValidator(constraint: Constraint.NumericConstraint)
      extends NumericConstraintValidator(constraint) {
    protected[this] def check(actualValue: BigDecimal, constraintValue: BigDecimal): Boolean =
      actualValue < constraintValue
  }

  private final class MultipleOfValidator(constraint: Constraint.NumericConstraint, multipleOfBigDecimal: Boolean)
      extends NumericConstraintValidator(constraint) {
    protected[this] def check(actualValue: BigDecimal, constraintValue: BigDecimal): Boolean = {
      val result = (actualValue / constraintValue).isWhole

      if (multipleOfBigDecimal) {
        result
      } else {
        result && !(actualValue.doubleValue / constraintValue.doubleValue).isInfinity
      }
    }
  }

  private abstract class StringConstraintValidator(constraint: Constraint.StringConstraint) extends Validator {
    protected[this] def check(actualValue: String): Boolean

    final def apply(c: HCursor): Vector[ValidationError] = c.value.asString match {
      case Some(asString) =>
        if (check(asString)) {
          Vector.empty
        } else {
          Vector(StringConstraintError(c.history, asString, constraint))
        }

      case None => Vector.empty
    }
  }

  private final class PatternValidator(constraint: Constraint.Pattern) extends StringConstraintValidator(constraint) {
    val compiled = Pattern.compile(constraint.regex)

    protected[this] def check(actualValue: String): Boolean = compiled.matcher(actualValue).find
  }

  private final class FormatValidator(constraint: Constraint.Format, formatValidator: ValidationFormatValidator)
      extends StringConstraintValidator(constraint) {
    protected[this] def check(actualValue: String): Boolean = formatValidator.isValid(constraint.format, actualValue)
  }

  private final class ContentMediaTypeValidator(
    constraint: Constraint.ContentMediaType,
    mediaTypeValidator: MediaTypeValidator,
    encodingValidator: EncodingValidator
  ) extends StringConstraintValidator(constraint) {
    protected[this] def check(actualValue: String): Boolean =
      mediaTypeValidator.isValid(
        constraint.value,
        constraint.encoding.flatMap(encodingValidator.decode(_, actualValue)).getOrElse(actualValue)
      )
  }

  private final class ContentEncodingValidator(
    constraint: Constraint.ContentEncoding,
    encodingValidator: EncodingValidator
  ) extends StringConstraintValidator(constraint) {
    protected[this] def check(actualValue: String): Boolean = encodingValidator.isValid(constraint.value, actualValue)
  }

  private final class MinLengthValidator(constraint: Constraint.MinLength)
      extends StringConstraintValidator(constraint) {
    protected[this] def check(actualValue: String): Boolean =
      actualValue.codePointCount(0, actualValue.length) >= constraint.value
  }

  private final class MaxLengthValidator(constraint: Constraint.MaxLength)
      extends StringConstraintValidator(constraint) {
    protected[this] def check(actualValue: String): Boolean =
      actualValue.codePointCount(0, actualValue.length) <= constraint.value
  }

  private final class UniqueItemsValidator(constraint: Constraint.ArrayConstraint[Schema.Resolved], eqJson: Eq[Json])
      extends Validator {
    final def apply(c: HCursor): Vector[ValidationError] = c.values match {
      case Some(valuesIterable) =>
        val values = valuesIterable.toVector
        val unique = 0.until(values.length).forall { i =>
          (i + 1).until(values.length).forall { j =>
            eqJson.neqv(values(i), values(j))
          }
        }

        if (unique) {
          Vector.empty
        } else {
          Vector(ArrayConstraintError(c.history, constraint))
        }
      case None => Vector.empty
    }
  }

  private final class MinItemsValidator(constraint: Constraint.MinItems) extends Validator {
    final def apply(c: HCursor): Vector[ValidationError] = c.values match {
      case Some(valuesIterable) =>
        if (valuesIterable.size >= constraint.value) {
          Vector.empty
        } else {
          Vector(ArrayConstraintError(c.history, constraint))
        }
      case None => Vector.empty
    }
  }

  private final class MaxItemsValidator(constraint: Constraint.MaxItems) extends Validator {
    final def apply(c: HCursor): Vector[ValidationError] = c.values match {
      case Some(valuesIterable) =>
        if (valuesIterable.size <= constraint.value) {
          Vector.empty
        } else {
          Vector(ArrayConstraintError(c.history, constraint))
        }
      case None => Vector.empty
    }
  }

  private final class ContainsValidator(constraint: Constraint.Contains[Schema.Resolved], compiler: Compiler)
      extends Validator {
    private[this] val compiled: Validator = compiler(constraint.schema)

    final def apply(c: HCursor): Vector[ValidationError] = c.values match {
      case Some(valuesIterable) =>
        if (valuesIterable.toVector.exists(compiled.isValid)) {
          Vector.empty
        } else {
          Vector(ArrayConstraintError(c.history, constraint))
        }
      case None => Vector.empty
    }
  }

  private final class ItemsTypeValidator(schema: Schema.Resolved, startIndex: Int, compiler: Compiler)
      extends Validator {
    private[this] val compiled: Validator = compiler(schema)

    final def apply(c: HCursor): Vector[ValidationError] = c.values match {
      case Some(valuesIterable) =>
        startIndex.until(valuesIterable.size).flatMap(n => c.downN(n).success).flatMap(compiled).toVector
      case None => Vector.empty
    }
  }

  private final class ItemsTupleValidator(schemas: Vector[Schema.Resolved], compiler: Compiler) extends Validator {
    private[this] val compiledWithIndices: Vector[(Validator, Int)] = schemas.map(compiler(_)).zipWithIndex

    final def apply(c: HCursor): Vector[ValidationError] = c.values match {
      case Some(valuesIterable) =>
        compiledWithIndices.flatMap { case (validator, n) =>
          val ac = c.downN(n)
          ac.success match {
            case None     => Vector.empty
            case Some(cc) => validator(cc)
          }
        }
      case None => Vector.empty
    }
  }

  private final class PropertiesValidator(values: Vector[Property[Schema.Resolved]], compiler: Compiler)
      extends Validator {
    private[this] val compiled: Map[String, Validator] = values.map { case Property(name, schema) =>
      (name, compiler(schema))
    }.toMap

    final def apply(c: HCursor): Vector[ValidationError] = c.keys match {
      case Some(keys) =>
        keys.flatMap { key =>
          compiled.get(key) match {
            case Some(validator) =>
              c.downField(key).success match {
                case Some(cc) => validator(cc)
                case None     => Vector.empty
              }
            case None => Vector.empty
          }
        }.toVector
      case None => Vector.empty
    }
  }

  private final class RequiredValidator(names: Vector[String]) extends Validator {
    final def apply(c: HCursor): Vector[ValidationError] = c.keys match {
      case Some(keys) => names.filterNot(keys.toSet).map(ObjectRequiredPropertyError(c.history, _))
      case None       => Vector.empty
    }
  }

  private final class PropertyNamesValidator(schema: Schema.Resolved, compiler: Compiler) extends Validator {
    private[this] val compiled: Validator = compiler(schema)

    final def apply(c: HCursor): Vector[ValidationError] = c.keys match {
      case Some(keys) =>
        keys.flatMap(name => compiled(Json.fromString(name).hcursor).map(_.withHistory(c.history))).toVector
      case None => Vector.empty
    }
  }

  private final class MinPropertiesValidator(limit: Int) extends Validator {
    final def apply(c: HCursor): Vector[ValidationError] = c.keys match {
      case Some(keys) =>
        val size = keys.size

        if (size >= limit) {
          Vector.empty
        } else {
          Vector(ObjectMinPropertiesError(c.history, size, limit))
        }
      case None => Vector.empty
    }
  }

  private final class MaxPropertiesValidator(limit: Int) extends Validator {
    final def apply(c: HCursor): Vector[ValidationError] = c.keys match {
      case Some(keys) =>
        val size = keys.size

        if (size <= limit) {
          Vector.empty
        } else {
          Vector(ObjectMaxPropertiesError(c.history, size, limit))
        }
      case None => Vector.empty
    }
  }

  private final class PatternPropertiesValidator(values: Vector[(String, Schema.Resolved)], compiler: Compiler)
      extends Validator {
    private[this] val compiled: Vector[(Pattern, Validator)] =
      values.map(p => (Pattern.compile(p._1), compiler(p._2)))

    final def apply(c: HCursor): Vector[ValidationError] = c.keys match {
      case Some(keys) =>
        keys.flatMap { key =>
          compiled.flatMap { case (pattern, validator) =>
            if (pattern.matcher(key).find) {
              c.downField(key).success match {
                case Some(cc) => validator(cc)
                case None     => Vector.empty
              }
            } else {
              Vector.empty
            }
          }
        }.toVector
      case None => Vector.empty
    }
  }

  private final class AdditionalPropertiesValidator(
    schema: Schema.Resolved,
    knownNames: Set[String],
    patterns: Set[String],
    compiler: Compiler
  ) extends Validator {
    private[this] val compiled: Validator = compiler(schema)
    private[this] val compiledPatterns = patterns.map(Pattern.compile)

    final def apply(c: HCursor): Vector[ValidationError] = c.keys match {
      case Some(keys) =>
        keys
          .filterNot(key => knownNames(key) || compiledPatterns.exists(_.matcher(key).find))
          .flatMap { key =>
            c.downField(key).success match {
              case Some(cc) => compiled(cc)
              case None     => Vector.empty
            }
          }
          .toVector
      case None => Vector.empty
    }
  }

  private final class DependenciesValidator(dependencies: Vector[Dependency[Schema.Resolved]], compiler: Compiler)
      extends Validator {
    import cats.syntax.functor._

    private[this] val compiled: Vector[Dependency[Validator]] = dependencies.map(_.map(compiler(_)))

    final def apply(c: HCursor): Vector[ValidationError] = c.keys match {
      case Some(keys) =>
        val keySet = keys.toSet

        compiled.filter(dependency => keySet(dependency.name)).flatMap {
          case Dependency.Property(_, required) =>
            required.filterNot(keySet).map(ObjectRequiredPropertyError(c.history, _))
          case Dependency.Schema(_, validator) => validator(c)
        }

      case None => Vector.empty
    }
  }

  private final class ConditionalValidator(
    ifSchema: Schema.Resolved,
    thenSchema: Option[Schema.Resolved],
    elseSchema: Option[Schema.Resolved],
    compiler: Compiler
  ) extends Validator {
    private[this] val compiledIf: Validator = compiler(ifSchema)
    private[this] val compiledThen = thenSchema.map(compiler(_))
    private[this] val compiledElse = elseSchema.map(compiler(_))

    final def apply(c: HCursor): Vector[ValidationError] = {
      if (compiledIf(c).isEmpty) {
        compiledThen.fold(Vector.empty[ValidationError])(_(c))
      } else {
        compiledElse.fold(Vector.empty[ValidationError])(_(c))
      }
    }
  }
}

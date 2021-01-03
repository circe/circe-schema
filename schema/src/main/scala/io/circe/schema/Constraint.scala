package io.circe.schema

import cats.{Applicative, Traverse}
import cats.data.Validated
import cats.kernel.Monoid
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.traverse._
import io.circe.{Decoder, DecodingFailure, HCursor, Json}
import io.circe.schema.{Format => SchemaFormat}
import scala.collection.immutable.ListMap

sealed trait Constraint[+S] {
  def compatible: Set[Type]
}

object Constraint {
  implicit val traverseConstraint: Traverse[Constraint] = new DefaultFoldable[Constraint] with Traverse[Constraint] {
    def traverse[G[_], A, B](fa: Constraint[A])(f: A => G[B])(implicit G: Applicative[G]): G[Constraint[B]] =
      fa match {
        case AllOf(schemas)                      => schemas.traverse(f).map(AllOf(_))
        case AnyOf(schemas)                      => schemas.traverse(f).map(AnyOf(_))
        case OneOf(schemas)                      => schemas.traverse(f).map(OneOf(_))
        case Not(schema)                         => f(schema).map(Not(_))
        case Contains(schema)                    => f(schema).map(Contains(_))
        case ItemsType(schema)                   => f(schema).map(ItemsType(_))
        case ItemsTuple(schemas)                 => schemas.traverse(f).map(ItemsTuple(_))
        case AdditionalItems(schema, startIndex) => f(schema).map(AdditionalItems(_, startIndex))
        case Properties(values)                  => values.traverse(_.traverse(f)).map(Properties(_))
        case PropertyNames(schema)               => f(schema).map(PropertyNames(_))
        case AdditionalProperties(schema, knownNames, patterns) =>
          f(schema).map(AdditionalProperties(_, knownNames, patterns))
        case Dependencies(values) => values.traverse(_.traverse(f)).map(Dependencies(_))
        case PatternProperties(values) =>
          values.traverse(pair => f(pair._2).map((pair._1, _))).map(PatternProperties(_))
        case Conditional(ifSchema, thenSchema, elseSchema) =>
          (f(ifSchema), thenSchema.traverse(f), elseSchema.traverse(f)).mapN(Conditional(_, _, _))

        case c @ Types(_)               => G.pure(c)
        case c @ Enumeration(_)         => G.pure(c)
        case c @ Minimum(_)             => G.pure(c)
        case c @ Maximum(_)             => G.pure(c)
        case c @ ExclusiveMinimum(_)    => G.pure(c)
        case c @ ExclusiveMaximum(_)    => G.pure(c)
        case c @ MultipleOf(_)          => G.pure(c)
        case c @ Pattern(_)             => G.pure(c)
        case c @ Format(_)              => G.pure(c)
        case c @ ContentMediaType(_, _) => G.pure(c)
        case c @ ContentEncoding(_)     => G.pure(c)
        case c @ MinLength(_)           => G.pure(c)
        case c @ MaxLength(_)           => G.pure(c)
        case c @ UniqueItems(_)         => G.pure(c)
        case c @ MinItems(_)            => G.pure(c)
        case c @ MaxItems(_)            => G.pure(c)
        case c @ Required(_)            => G.pure(c)
        case c @ MinProperties(_)       => G.pure(c)
        case c @ MaxProperties(_)       => G.pure(c)
      }

    override def foldMap[A, B](fa: Constraint[A])(f: A => B)(implicit B: Monoid[B]): B =
      fa match {
        case AllOf(schemas)                                     => schemas.foldMap(f)
        case AnyOf(schemas)                                     => schemas.foldMap(f)
        case OneOf(schemas)                                     => schemas.foldMap(f)
        case Not(schema)                                        => f(schema)
        case Contains(schema)                                   => f(schema)
        case ItemsType(schema)                                  => f(schema)
        case ItemsTuple(schemas)                                => schemas.foldMap(f)
        case AdditionalItems(schema, startIndex)                => f(schema)
        case Properties(values)                                 => values.foldMap(_.foldMap(f))
        case PropertyNames(schema)                              => f(schema)
        case AdditionalProperties(schema, knownNames, patterns) => f(schema)
        case Dependencies(values)                               => values.foldMap(_.foldMap(f))
        case PatternProperties(values)                          => values.foldMap(pair => f(pair._2))
        case Conditional(ifSchema, thenSchema, elseSchema) =>
          B.combine(f(ifSchema), B.combine(thenSchema.foldMap(f), elseSchema.foldMap(f)))
        case _ => B.empty
      }
  }

  private val allTypes: Set[Type] =
    Set(Type.Array, Type.Boolean, Type.Integer, Type.Null, Type.Number, Type.Object, Type.String)
  private val numericTypes: Set[Type] = Set(Type.Integer, Type.Number)
  private val stringTypes: Set[Type] = Set(Type.String)
  private val arrayTypes: Set[Type] = Set(Type.Array)
  private val objectTypes: Set[Type] = Set(Type.Object)

  sealed abstract class AnyTypeConstraint[+S] extends Constraint[S] {
    final def compatible: Set[Type] = allTypes
  }

  sealed abstract class NumericConstraint extends Constraint[Nothing] {
    def value: BigDecimal
    final def compatible: Set[Type] = numericTypes
  }

  sealed abstract class StringConstraint extends Constraint[Nothing] {
    final def compatible: Set[Type] = stringTypes
  }

  sealed abstract class ArrayConstraint[+S] extends Constraint[S] {
    final def compatible: Set[Type] = arrayTypes
  }

  sealed abstract class ObjectConstraint[+S] extends Constraint[S] {
    final def compatible: Set[Type] = objectTypes
  }

  case class Types(values: Vector[Type]) extends Constraint[Nothing] {
    val compatible: Set[Type] = values.toSet
  }

  case class Enumeration(values: Vector[Json]) extends AnyTypeConstraint[Nothing]
  case class AllOf[+S](schemas: Vector[S]) extends AnyTypeConstraint[S]
  case class AnyOf[+S](schemas: Vector[S]) extends AnyTypeConstraint[S]
  case class OneOf[+S](schemas: Vector[S]) extends AnyTypeConstraint[S]
  case class Not[+S](schema: S) extends AnyTypeConstraint[S]

  case class Minimum(value: BigDecimal) extends NumericConstraint
  case class Maximum(value: BigDecimal) extends NumericConstraint
  case class ExclusiveMinimum(value: BigDecimal) extends NumericConstraint
  case class ExclusiveMaximum(value: BigDecimal) extends NumericConstraint
  case class MultipleOf(value: BigDecimal) extends NumericConstraint

  case class Pattern(regex: String) extends StringConstraint
  case class Format(format: SchemaFormat) extends StringConstraint
  case class ContentMediaType(value: String, encoding: Option[Encoding]) extends StringConstraint
  case class ContentEncoding(value: Encoding) extends StringConstraint
  case class MinLength(value: Int) extends StringConstraint
  case class MaxLength(value: Int) extends StringConstraint

  case class UniqueItems(value: Boolean) extends ArrayConstraint[Nothing]
  case class MinItems(value: Int) extends ArrayConstraint[Nothing]
  case class MaxItems(value: Int) extends ArrayConstraint[Nothing]
  case class Contains[+S](schema: S) extends ArrayConstraint[S]
  case class ItemsType[+S](schema: S) extends ArrayConstraint[S]
  case class ItemsTuple[+S](schemas: Vector[S]) extends ArrayConstraint[S]
  case class AdditionalItems[+S](schema: S, startIndex: Int) extends ArrayConstraint[S]

  case class Properties[+S](values: Vector[Property[S]]) extends ObjectConstraint[S]
  case class Required(names: Vector[String]) extends ObjectConstraint[Nothing]
  case class PropertyNames[+S](schema: S) extends ObjectConstraint[S]
  case class MinProperties(value: Int) extends ObjectConstraint[Nothing]
  case class MaxProperties(value: Int) extends ObjectConstraint[Nothing]
  case class AdditionalProperties[S](schema: S, knownNames: Set[String], patterns: Set[String])
      extends ObjectConstraint[S]
  case class Dependencies[+S](values: Vector[Dependency[S]]) extends ObjectConstraint[S]
  case class PatternProperties[+S](values: Vector[(String, S)]) extends ObjectConstraint[S]

  case class Conditional[+S](ifSchema: S, thenSchema: Option[S], elseSchema: Option[S]) extends AnyTypeConstraint[S]

  private def propertyFromPair[S](p: (String, S)): Property[S] = Property(p._1, p._2)
  private lazy val decodeSchemaVector: Decoder[Vector[Schema.Unresolved]] = Decoder.decodeVector[Schema.Unresolved]
  private lazy val decodeSchemaListMap: Decoder[ListMap[String, Schema.Unresolved]] =
    Decoder[ListMap[String, Schema.Unresolved]]

  private val decodeTypeSingle: Decoder[Constraint[Schema.Unresolved]] =
    Decoder[Type].map(value => Types(Vector(value)))
  private val decodeTypeArray: Decoder[Constraint[Schema.Unresolved]] = Decoder[Vector[Type]].map(Types(_))
  private val decodeType: Decoder[Constraint[Schema.Unresolved]] = decodeTypeSingle.or(decodeTypeArray)
  private val decodeEnum: Decoder[Constraint[Schema.Unresolved]] = Decoder[Vector[Json]].map(Enumeration(_))
  private val decodeConst: Decoder[Constraint[Schema.Unresolved]] =
    Decoder[Json].map(value => Enumeration(Vector(value)))

  private lazy val decodeAllOf: Decoder[Constraint[Schema.Unresolved]] = decodeSchemaVector.map(AllOf(_))
  private lazy val decodeAnyOf: Decoder[Constraint[Schema.Unresolved]] = decodeSchemaVector.map(AnyOf(_))
  private lazy val decodeOneOf: Decoder[Constraint[Schema.Unresolved]] = decodeSchemaVector.map(OneOf(_))
  private lazy val decodeNot: Decoder[Constraint[Schema.Unresolved]] = Decoder[Schema.Unresolved].map(Not(_))

  private val decodeMinimum: Decoder[Constraint[Schema.Unresolved]] = Decoder[BigDecimal].map(Minimum(_))
  private val decodeMaximum: Decoder[Constraint[Schema.Unresolved]] = Decoder[BigDecimal].map(Maximum(_))
  private val decodeExclusiveMinimum: Decoder[Constraint[Schema.Unresolved]] =
    Decoder[BigDecimal].map(ExclusiveMinimum(_))
  private val decodeExclusiveMaximum: Decoder[Constraint[Schema.Unresolved]] =
    Decoder[BigDecimal].map(ExclusiveMaximum(_))
  private val decodeMultipleOf: Decoder[Constraint[Schema.Unresolved]] = Decoder[BigDecimal].map(MultipleOf(_))

  private val decodePattern: Decoder[Constraint[Schema.Unresolved]] = Decoder[String].map(Pattern(_))
  private val decodeFormat: Decoder[Constraint[Schema.Unresolved]] = Decoder[SchemaFormat].map(Format(_))
  private val decodeContentMediaType: Decoder[Constraint[Schema.Unresolved]] =
    Decoder[String].map(ContentMediaType(_, None))
  private val decodeContentEncoding: Decoder[Constraint[Schema.Unresolved]] =
    Decoder[Encoding].map(ContentEncoding(_))
  private val decodeMinLength: Decoder[Constraint[Schema.Unresolved]] = Decoder[Int].map(MinLength(_))
  private val decodeMaxLength: Decoder[Constraint[Schema.Unresolved]] = Decoder[Int].map(MaxLength(_))

  private val decodeUniqueItems: Decoder[Constraint[Schema.Unresolved]] = Decoder[Boolean].map(UniqueItems(_))
  private val decodeMinItems: Decoder[Constraint[Schema.Unresolved]] = Decoder[Int].map(MinItems(_))
  private val decodeMaxItems: Decoder[Constraint[Schema.Unresolved]] = Decoder[Int].map(MaxItems(_))
  private lazy val decodeContains: Decoder[Constraint[Schema.Unresolved]] = Decoder[Schema.Unresolved].map(Contains(_))
  private lazy val decodeItemsType: Decoder[Constraint[Schema.Unresolved]] =
    Decoder[Schema.Unresolved].map(ItemsType(_))
  private lazy val decodeItemsTuple: Decoder[Constraint[Schema.Unresolved]] = decodeSchemaVector.map(ItemsTuple(_))
  private lazy val decodeItems: Decoder[Constraint[Schema.Unresolved]] = decodeItemsType.or(decodeItemsTuple)
  // We initialize `startIndex` to `Int.MaxValue`, since it depends on the `items` field.
  private lazy val decodeAdditionalItems: Decoder[Constraint[Schema.Unresolved]] =
    Decoder[Schema.Unresolved].map(AdditionalItems(_, Int.MaxValue))

  private lazy val decodeProperties: Decoder[Constraint[Schema.Unresolved]] =
    decodeSchemaListMap.map(pairs => Properties(pairs.map(propertyFromPair).toVector))
  private val decodeRequired: Decoder[Constraint[Schema.Unresolved]] = Decoder[Vector[String]].map(Required(_))
  private lazy val decodePropertyNames: Decoder[Constraint[Schema.Unresolved]] =
    Decoder[Schema.Unresolved].map(PropertyNames(_))
  private val decodeMinProperties: Decoder[Constraint[Schema.Unresolved]] = Decoder[Int].map(MinProperties(_))
  private val decodeMaxProperties: Decoder[Constraint[Schema.Unresolved]] = Decoder[Int].map(MaxProperties(_))
  private lazy val decodeAdditionalProperties: Decoder[Constraint[Schema.Unresolved]] =
    Decoder[Schema.Unresolved].map(AdditionalProperties(_, Set.empty, Set.empty))
  private lazy val decodePatternProperties: Decoder[Constraint[Schema.Unresolved]] =
    decodeSchemaListMap.map(pairs => PatternProperties(pairs.toVector))
  private lazy val decodeDependencies: Decoder[Constraint[Schema.Unresolved]] =
    Dependency.decodeDependencies.map(Dependencies(_))
  private lazy val decodeConditional: Decoder[Constraint[Schema.Unresolved]] =
    Decoder[Schema.Unresolved].map(Conditional(_, None, None))

  private lazy val decodeThenSchema: Decoder[Option[Schema.Unresolved]] = Decoder[Option[Schema.Unresolved]].at("then")
  private lazy val decodeElseSchema: Decoder[Option[Schema.Unresolved]] = Decoder[Option[Schema.Unresolved]].at("else")

  val decodeConstraints: Decoder[Vector[Constraint[Schema.Unresolved]]] =
    new Decoder[Vector[Constraint[Schema.Unresolved]]] {
      private[this] lazy val decoders: Map[String, Decoder[Constraint[Schema.Unresolved]]] = Map(
        "type" -> decodeType,
        "enum" -> decodeEnum,
        "allOf" -> decodeAllOf,
        "anyOf" -> decodeAnyOf,
        "oneOf" -> decodeOneOf,
        "not" -> decodeNot,
        "const" -> decodeConst,
        "minimum" -> decodeMinimum,
        "maximum" -> decodeMaximum,
        "exclusiveMinimum" -> decodeExclusiveMinimum,
        "exclusiveMaximum" -> decodeExclusiveMaximum,
        "multipleOf" -> decodeMultipleOf,
        "pattern" -> decodePattern,
        "format" -> decodeFormat,
        "contentMediaType" -> decodeContentMediaType,
        "contentEncoding" -> decodeContentEncoding,
        "minLength" -> decodeMinLength,
        "maxLength" -> decodeMaxLength,
        "uniqueItems" -> decodeUniqueItems,
        "minItems" -> decodeMinItems,
        "maxItems" -> decodeMaxItems,
        "contains" -> decodeContains,
        "items" -> decodeItems,
        "additionalItems" -> decodeAdditionalItems,
        "properties" -> decodeProperties,
        "required" -> decodeRequired,
        "propertyNames" -> decodePropertyNames,
        "minProperties" -> decodeMinProperties,
        "maxProperties" -> decodeMaxProperties,
        "additionalProperties" -> decodeAdditionalProperties,
        "patternProperties" -> decodePatternProperties,
        "dependencies" -> decodeDependencies,
        "if" -> decodeConditional
      )

      private def knownKeys(keys: Iterable[String]): Vector[String] = keys.filter(decoders.contains).toVector

      def apply(c: HCursor): Decoder.Result[Vector[Constraint[Schema.Unresolved]]] = c.keys match {
        case None       => Left(DecodingFailure("Constraints", c.history))
        case Some(keys) => knownKeys(keys).traverse(key => c.get(key)(decoders(key)))
      }

      override def decodeAccumulating(c: HCursor): Decoder.AccumulatingResult[Vector[Constraint[Schema.Unresolved]]] =
        c.keys match {
          case None => Validated.invalidNel(DecodingFailure("Constraints", c.history))
          case Some(keys) =>
            keys
              .filter(decoders.contains)
              .toVector
              .traverse(key => decoders(key).tryDecodeAccumulating(c.downField(key)))
        }
    }.flatMap { constraints =>
      val maybeIfSchema = constraints.collectFirst {
        case Conditional(ifSchema, None, None) => ifSchema
      }

      maybeIfSchema match {
        case Some(ifSchema) =>
          decodeThenSchema.product(decodeElseSchema).map {
            case (thenSchema, elseSchema) =>
              constraints :+ Conditional(ifSchema, thenSchema, elseSchema)
          }
        case None => Decoder.const(constraints)
      }
    }

  // This thing is generic but I don't know of a good place to get it.
  private abstract class DefaultFoldable[F[_]] { self: Traverse[F] =>
    import cats.Eval
    import cats.arrow.Category
    import cats.data.Chain

    final def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      foldMap[A, Eval[B] => Eval[B]](fa)(a => lbb => Eval.defer(f(a, lbb)))(Category[Function1].algebra)(lb)

    final def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
      foldMap[A, Chain[A]](fa)(Chain.one).foldLeft(b)(f)
  }
}

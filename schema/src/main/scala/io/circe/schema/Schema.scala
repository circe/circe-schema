package io.circe.schema

import cats.Eval
import cats.kernel.Monoid
import io.circe.{Decoder, HCursor}
import java.net.{URI, URISyntaxException}

sealed trait Schema[+R] {
  def metadata: Metadata
  def references: Vector[R]
  def unresolved: Boolean
}

object Schema {
  type Unresolved = Schema[URI]
  type Resolved = Schema[Nothing]

  case object RejectAll extends Schema[Nothing] {
    def metadata: Metadata = Metadata.empty
    def references: Vector[Nothing] = Vector.empty
    def unresolved: Boolean = false
  }

  case object AcceptAll extends Schema[Nothing] {
    def metadata: Metadata = Metadata.empty
    def references: Vector[Nothing] = Vector.empty
    def unresolved: Boolean = false
  }

  case class Ref[+R](cursor: HCursor, value: R) extends Schema[R] {
    def metadata: Metadata = Metadata.empty
    def references: Vector[R] = Vector(value)
    def unresolved: Boolean = true
  }

  case class Link(schema: Eval[Resolved]) extends Schema[Nothing] {
    def metadata: Metadata = Metadata.empty
    def references: Vector[Nothing] = Vector.empty
    def unresolved: Boolean = false
  }

  private[this] val disjunctionMonoid: Monoid[Boolean] = Monoid.instance(false, _ || _)

  case class Constraints[R](metadata: Metadata, constraints: Vector[Constraint[Schema[R]]]) extends Schema[R] {
    import cats.syntax.foldable._

    def references: Vector[R] = constraints.foldMap(_.foldMap(_.references))
    def unresolved: Boolean = constraints.foldMap(_.foldMap(_.unresolved)(disjunctionMonoid))(disjunctionMonoid)
  }

  private def postProcessConstraints[R](constraints: Vector[Constraint[Schema[R]]]): Vector[Constraint[Schema[R]]] = {
    val encoding: Option[Encoding] = constraints.collectFirst { case Constraint.ContentEncoding(value) =>
      value
    }

    val newConstraints = encoding match {
      case Some(encoding) =>
        constraints.map {
          case Constraint.ContentMediaType(value, _) => Constraint.ContentMediaType(value, Some(encoding))
          case other                                 => other
        }
      case None => constraints
    }

    val additionalItemsIndex = newConstraints.indexWhere {
      case Constraint.AdditionalItems(_, _) => true
      case _                                => false
    }

    val constraintsWithAdditionalItems = if (additionalItemsIndex == -1) {
      newConstraints
    } else {
      val itemsTupleSize = newConstraints.collectFirst { case Constraint.ItemsTuple(schemas) =>
        schemas.size
      }

      itemsTupleSize match {
        case Some(size) =>
          val newValue = newConstraints(additionalItemsIndex) match {
            case Constraint.AdditionalItems(schema, _) => Constraint.AdditionalItems(schema, size)
            case other                                 => other
          }
          newConstraints.updated(additionalItemsIndex, newValue)
        case None =>
          newConstraints.patch(additionalItemsIndex, Nil, 1)
      }
    }

    val additionalPropertiesIndex = constraintsWithAdditionalItems.indexWhere {
      case Constraint.AdditionalProperties(_, _, _) => true
      case _                                        => false
    }

    if (additionalPropertiesIndex == -1) {
      constraintsWithAdditionalItems
    } else {
      val maybeKnownNames = constraintsWithAdditionalItems.collectFirst { case Constraint.Properties(values) =>
        values.map(_.name).toSet
      }

      val maybePatterns = constraintsWithAdditionalItems.collectFirst { case Constraint.PatternProperties(values) =>
        values.map(_._1).toSet
      }

      if (maybeKnownNames.isEmpty && maybePatterns.isEmpty) {
        constraintsWithAdditionalItems
      } else {
        constraintsWithAdditionalItems(additionalPropertiesIndex) match {
          case original @ Constraint.AdditionalProperties(_, _, _) =>
            val withKnownNames = maybeKnownNames.fold(original)(knownNames => original.copy(knownNames = knownNames))
            val withPatterns =
              maybePatterns.fold(withKnownNames)(patterns => withKnownNames.copy(patterns = patterns))
            constraintsWithAdditionalItems.updated(additionalPropertiesIndex, withPatterns)
          case _ => constraintsWithAdditionalItems
        }
      }
    }
  }

  private val decodeBooleanSchema: Decoder[Schema[URI]] =
    Decoder[Boolean].map(value => if (value) AcceptAll else RejectAll)

  private val decodeRef: Decoder[Schema[URI]] =
    Decoder[HCursor].product(Decoder[String].at("$ref")).emap { case (cursor, value) =>
      try {
        Right(Ref(cursor, new URI(value)): Schema[URI])
      } catch {
        case e: URISyntaxException => Left(e.getReason)
      }
    }

  private lazy val decodeConstraints: Decoder[Schema[URI]] =
    Decoder[Metadata].product(Constraint.decodeConstraints).map { case (metadata, constraints) =>
      Constraints(metadata, postProcessConstraints(constraints))
    }

  implicit lazy val decodeSchema: Decoder[Schema[URI]] = decodeRef.or(decodeConstraints).or(decodeBooleanSchema)
}

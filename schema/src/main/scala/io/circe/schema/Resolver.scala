package io.circe.schema

import cats.{Applicative, ApplicativeError, Eval, Monad}
import cats.data.StateT
import cats.syntax.functor._
import cats.syntax.traverse._
import io.circe.{Decoder, DecodingFailure, HCursor}
import io.circe.pointer.{Pointer, PointerFailure, PointerSyntaxError}
import java.net.URI

abstract class Resolver[F[_], R](implicit private val F: Applicative[F])
    extends (Schema[R] => F[Eval[Schema[Nothing]]]) {
  protected[this] def resolve(cursor: HCursor, uri: R): F[Eval[Schema.Resolved]]

  final def apply(schema: Schema[R]): F[Eval[Schema[Nothing]]] = schema match {
    case Schema.RejectAll          => F.pure(Eval.now(Schema.RejectAll))
    case Schema.AcceptAll          => F.pure(Eval.now(Schema.AcceptAll))
    case Schema.Ref(cursor, value) => F.map(resolve(cursor, value))(es => es.map(_ => Schema.Link(es)))
    case Schema.Link(schema)       => F.pure(schema)
    case Schema.Constraints(metadata, constraints) =>
      F.map(constraints.traverse(_.traverse(apply(_))))((cs: Vector[Constraint[Eval[Schema.Resolved]]]) =>
        cs.traverse(_.sequence).map(Schema.Constraints(metadata, _))
      )
  }
}

abstract class CachingUriResolver[F[_]](implicit F: Monad[F]) extends Resolver[StateT[F, Resolver.Cache, *], URI] {}

object Resolver {
  type Cache = Map[Pointer, Eval[Schema.Resolved]]
  type Result[A] = Either[Failure, A]
  type StateResult[A] = StateT[Result, Cache, A]

  sealed abstract class Failure extends Exception

  object Failure {
    case class Unresolvable(uri: URI) extends Failure
    case class InvalidPointerSyntax(error: PointerSyntaxError) extends Failure
    case class InvalidLocation(failure: PointerFailure) extends Failure
    case class InvalidSchema(failure: DecodingFailure) extends Failure

    def unresolvable[F[_], A](uri: URI)(implicit F: ApplicativeError[F, Failure]): F[A] =
      F.raiseError(Unresolvable(uri))
    def invalidPointerSyntax[F[_], A](error: PointerSyntaxError)(implicit F: ApplicativeError[F, Failure]): F[A] =
      F.raiseError(InvalidPointerSyntax(error))
    def invalidLocation[F[_], A](failure: PointerFailure)(implicit F: ApplicativeError[F, Failure]): F[A] =
      F.raiseError(InvalidLocation(failure))
    def invalidSchema[F[_], A](failure: DecodingFailure)(implicit F: ApplicativeError[F, Failure]): F[A] =
      F.raiseError(InvalidSchema(failure))
  }

  private def link(schema: Schema.Resolved): Eval[Schema.Resolved] = schema match {
    case Schema.RejectAll                              => Eval.now(Schema.RejectAll)
    case Schema.AcceptAll                              => Eval.now(Schema.AcceptAll)
    case Schema.Ref(_, value)                          => value
    case s @ Schema.Link(schema)                       => schema
    case s @ Schema.Constraints(metadata, constraints) => Eval.now(s)
  }

  // A simple resolver that only works with pointers into the current document.
  val local: Resolver[StateResult, URI] = new CachingUriResolver[Result] {
    def resolve(cursor: HCursor, uri: URI): StateResult[Eval[Schema.Resolved]] = {
      if (uri.getScheme.eq(null) && uri.getSchemeSpecificPart.isEmpty) {
        val fragment = Option(uri.getFragment).getOrElse("")

        Pointer.parse(fragment) match {
          case Right(pointer) =>
            StateT.inspect[Result, Cache, Option[Eval[Schema.Resolved]]](_.get(pointer)).flatMap {
              case Some(cached) => StateT.pure(cached)
              case None =>
                val cc = pointer(cursor)

                StateT
                  .liftF[Result, Cache, HCursor](
                    cc.success.toRight(Failure.InvalidLocation(PointerFailure(cc.history)))
                  )
                  .flatMap { hc =>
                    Decoder[Schema[URI]].apply(hc) match {
                      case Right(schema) =>
                        //this(schema).flatMap {
                        //  case Schema.RejectAll          => F.pure(Schema.RejectAll)
                        //  case Schema.AcceptAll          => F.pure(Schema.AcceptAll)

                        this(schema).flatMap { resolved =>
                          StateT.modify[Result, Cache](_.updated(pointer, resolved)).as(resolved)
                        }
                      case Left(failure) => Failure.invalidSchema[StateResult, Eval[Schema.Resolved]](failure)
                    }
                  }
            }
          case Left(error) => Failure.invalidPointerSyntax[StateResult, Eval[Schema.Resolved]](error)
        }

      } else {
        Failure.unresolvable[StateResult, Eval[Schema.Resolved]](uri)
      }
    }
  }
}

package io.circe.schema

import cats.{Applicative, Eval, FlatMap, Traverse}
import io.circe.Decoder
import scala.annotation.tailrec
import scala.collection.immutable.ListMap

sealed trait Dependency[+S] {
  def name: String
}

object Dependency {
  import io.circe.schema.{Schema => CirceSchema}

  case class Property(name: String, other: Vector[String]) extends Dependency[Nothing]
  case class Schema[+S](name: String, schema: S) extends Dependency[S]

  implicit val traverseInstance: Traverse[Dependency] with FlatMap[Dependency] = new Traverse[Dependency]
    with FlatMap[Dependency] {
    def traverse[G[_], A, B](fa: Dependency[A])(f: A => G[B])(implicit G: Applicative[G]): G[Dependency[B]] =
      fa match {
        case p @ Property(_, _)   => G.pure(p)
        case Schema(name, schema) => G.map(f(schema))(Schema(name, _))
      }

    def foldLeft[A, B](fa: Dependency[A], b: B)(f: (B, A) => B): B = fa match {
      case p @ Property(_, _) => b
      case Schema(_, schema)  => f(b, schema)
    }

    def foldRight[A, B](fa: Dependency[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
      case p @ Property(_, _) => lb
      case Schema(_, schema)  => f(schema, lb)
    }

    def flatMap[A, B](fa: Dependency[A])(f: A => Dependency[B]): Dependency[B] = fa match {
      case p @ Property(_, _) => p
      case Schema(_, schema)  => f(schema)
    }

    @tailrec
    def tailRecM[A, B](a: A)(f: A => Dependency[Either[A, B]]): Dependency[B] = f(a) match {
      case p @ Property(_, _)     => p
      case Schema(name, Right(b)) => Schema(name, b)
      case Schema(_, Left(a))     => tailRecM(a)(f)
    }
  }

  private implicit lazy val decodeValue: Decoder[Either[Vector[String], CirceSchema.Unresolved]] =
    Decoder[Vector[String]].either(Decoder[CirceSchema.Unresolved])

  private[schema] lazy val decodeDependencies: Decoder[Vector[Dependency[CirceSchema.Unresolved]]] =
    Decoder[ListMap[String, Either[Vector[String], CirceSchema.Unresolved]]].map { pairs =>
      pairs.map {
        case (name, Right(schema)) => Schema(name, schema)
        case (name, Left(other))   => Property(name, other)
      }.toVector
    }
}

package io.circe.schema

import cats.{Applicative, Eval, FlatMap, Traverse}
import scala.annotation.tailrec

case class Property[+S](name: String, schema: S)

object Property {
  implicit val traverseProperty: Traverse[Property] with FlatMap[Property] = new Traverse[Property]
    with FlatMap[Property] {
    def traverse[G[_], A, B](fa: Property[A])(f: A => G[B])(implicit G: Applicative[G]): G[Property[B]] =
      G.map(f(fa.schema))(Property(fa.name, _))
    def foldLeft[A, B](fa: Property[A], b: B)(f: (B, A) => B): B = f(b, fa.schema)
    def foldRight[A, B](fa: Property[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = f(fa.schema, lb)
    def flatMap[A, B](fa: Property[A])(f: A => Property[B]): Property[B] = f(fa.schema)

    @tailrec
    def tailRecM[A, B](a: A)(f: A => Property[Either[A, B]]): Property[B] = f(a) match {
      case Property(name, Right(b)) => Property(name, b)
      case Property(_, Left(a))     => tailRecM(a)(f)
    }
  }
}

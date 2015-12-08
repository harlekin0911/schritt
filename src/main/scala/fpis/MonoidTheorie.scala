package fpis

/**
 * see: https://apocalisp.wordpress.com/2010/06/14/on-monoids/
 */

trait Monoid[T] {
    def append(m1: T, m2: T): T
    val identity: T
}

import scala.language.higherKinds
trait Category[F[_,_]] {
    def compose[A,B,C](f: F[B,C], g: F[A,B]): F[A,C]
    def identity[A]: F[A,A]
}

object  MonoidTheorie {

    val catScala = new Category[Function1] {
        def compose[A,B,C](f: B => C, g: A => B): A => C = f compose g
        def identity[A]: A => A = (a: A) => a
    }

    // The category for a given monoid.
    def monoidCategory[M](m: Monoid[M]) =  new Category[({type lambda[alpha, beta] = M})#lambda] {
        def identity[A] = m.identity
        def compose[X, Y, Z](f: M, g: M) = m.append(f, g)
    }
    
    implicitly[Int =:= Int]
   // implicitly[Int =:= String]
}




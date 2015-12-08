package fpis.state
//package functionalProgramming



trait RNG2 {
  def nextInt: (Int, RNG2) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG2 {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

 def nonNegativeInt(rng: RNG2): (Int, RNG2) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
    (i,r)
  }

  type Rand2[+A] = RNG2 => (A, RNG2)

  val int: Rand2[Int] = ( r : RNG2 ) => r.nextInt

  def unit[A](a: A): Rand2[A] = rng => (a, rng)

  def map[A,B](s: Rand2[A])(f: A => B): Rand2[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  val _double: Rand2[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  // This implementation of map2 passes the initial RNG to the first argument
  // and the resulting RNG to the second argument. It's not necessarily wrong
  // to do this the other way around, since the results are random anyway.
  // We could even pass the initial RNG to both `f` and `g`, but that might
  // have unexpected results. E.g. if both arguments are `RNG.int` then we would
  // always get two of the same `Int` in the result. When implementing functions
  // like this, it's important to consider how we would test them for
  // correctness.
  def map2[A,B,C](ra: Rand2[A], rb: Rand2[B])(f: (A, B) => C): Rand2[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

}


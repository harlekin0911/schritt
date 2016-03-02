package fpis




                            /*

  We now introduce a type, `Process`, representing pure, single-input
  stream transducers. It can be in of three states - it can be
  emitting a value to the output (`Emit`), reading a value from its
  input (`Await`) or signaling termination via `Halt`.

                             */

  sealed trait Process[I,O] {
    
    import Process._

    /*
     * A `Process[I,O]` can be used to transform a `Stream[I]` to a
     * `Stream[O]`.
     */
    def apply(s: Stream[I]): Stream[O] = this match {
      case Halt() => Stream()
      case Await(recv) => s match {
        case h #:: t => recv(Some(h))(t)
        case xs => recv(None)(xs) // Stream is empty
      }
      case Emit(h,t) => h #:: t(s)
    }

    /*
     * See `Process.lift` for a typical repeating `Process`
     * definition expressed with explicit recursion.
     */

    /*
     * `Process` definitions can often be expressed without explicit
     * recursion, by repeating some simpler `Process` forever.
     */
    def repeat: Process[I,O] = {
      def go(p: Process[I,O]): Process[I,O] = p match {
        case Halt() => go(this)
        case Await(recv) => Await {
          case None => recv(None)
          case i => go(recv(i))
        }
        case Emit(h, t) => Emit(h, go(t))
      }
      go(this)
    }

  }
  
  object Process {

    case class Emit[I,O](   head: O, tail: Process[I,O] = Halt[I,O]()) extends Process[I,O]
    case class Await[I,O](  recv: Option[I] => Process[I,O]) extends Process[I,O]
    case class Halt[I,O]() extends Process[I,O]

    def emit[I,O](head: O, tail: Process[I,O] = Halt[I,O]()): Process[I,O] = Emit(head, tail)

    /**
     * A helper function to await an element or fall back to another process
     * if there is no input.
     */
    def await[I,O](f: I => Process[I,O], fallback: Process[I,O] = Halt[I,O]()): Process[I,O] =
      Await[I,O] {
        case Some(i) => f(i)
        case None => fallback
      }

    /*
     * We can convert any function `f: I => O` to a `Process[I,O]`. We
     * simply `Await`, then `Emit` the value received, transformed by
     * `f`.
     */
    def liftOne[I,O](f: I => O): Process[I,O] = Await {
        case Some(i) => emit(f(i))
        case None => Halt()
    }

    def lift[I,O](f: I => O): Process[I,O] = liftOne(f).repeat

    /*
     * As an example of `repeat`, here's a definition of `filter` that
     * uses `repeat`.
     */
    def filter[I](f: I => Boolean): Process[I,I] =
      Await[I,I] {
        case Some(i) if f(i) => emit(i)
        case _ => Halt()
      }.repeat

    /*
     * Here's a typical `Process` definition that requires tracking some
     * piece of state (in this case, the running total):
     */
    def sum: Process[Double,Double] = {
      def go(acc: Double): Process[Double,Double] = await(d => emit(d+acc, go(d+acc)))
      go(0.0)
    }

    /*
     * Exercise 1: Implement `take`, `drop`, `takeWhile`, and `dropWhile`.
     */
    def take[I](n: Int): Process[I,I] =
      if (n <= 0) Halt()
      else await(i => emit(i, take[I](n-1)))

    def drop[I](n: Int): Process[I,I] =
      if (n <= 0) id
      else await(i => drop[I](n-1))

    def takeWhile[I](f: I => Boolean): Process[I,I] =
      await(i =>
        if (f(i)) emit(i, takeWhile(f))
        else      Halt())

    def dropWhile[I](f: I => Boolean): Process[I,I] =
      await(i =>
        if (f(i)) dropWhile(f)
        else      emit(i,id))

    /* The identity `Process`, just repeatedly echos its input. */
    def id[I]: Process[I,I] = lift(identity)

    /*
     * Exercise 2: Implement `count`.
     *
     * Here's one implementation, with three stages - we map all inputs
     * to 1.0, compute a running sum, then finally convert the output
     * back to `Int`. The three stages will be interleaved - as soon
     * as the first element is examined, it will be converted to 1.0,
     * then added to the running total, and then this running total
     * will be converted back to `Int`, then the `Process` will examine
     * the next element, and so on.
     */
    /* For comparison, here is an explicit recursive implementation. */
    def count2[I]: Process[I,Int] = {
      def go(n: Int): Process[I,Int] =
        await((i: I) => emit(n+1, go(n+1)))
      go(0)
    }

    /*
     * Exercise 3: Implement `mean`.
     *
     * This is an explicit recursive definition. We'll factor out a
     * generic combinator shortly.
     */
    def mean: Process[Double,Double] = {
      def go(sum: Double, count: Double): Process[Double,Double] =
        await((d: Double) => emit((sum+d) / (count+1), go(sum+d,count+1)))
      go(0.0, 0.0)
    }

    def loop[S,I,O](z: S)(f: (I,S) => (O,S)): Process[I,O] =
      await((i: I) => f(i,z) match {
        case (o,s2) => emit(o, loop(s2)(f))
      })

    /* Exercise 4: Implement `sum` and `count` in terms of `loop` */

    def sum2: Process[Double,Double] =
      loop(0.0)((d:Double, acc) => (acc+d,acc+d))

    def count3[I]: Process[I,Int] =
      loop(0)((_:I,n) => (n+1,n+1))

    /*
     * Exercise 7: Can you think of a generic combinator that would
     * allow for the definition of `mean` in terms of `sum` and
     * `count`?
     *
     * Yes, it is `zip`, which feeds the same input to two processes.
     * The implementation is a bit tricky, as we have to make sure
     * that input gets fed to both `p1` and `p2`.
     */
    def zip[A,B,C](p1: Process[A,B], p2: Process[A,C]): Process[A,(B,C)] =
      (p1, p2) match {
        case (Halt(), _) => Halt()
        case (_, Halt()) => Halt()
        case (Emit(b, t1), Emit(c, t2)) => Emit((b,c), zip(t1, t2))
        case (Await(recv1), _) =>
          Await((oa: Option[A]) => zip(recv1(oa), feed(oa)(p2)))
        case (_, Await(recv2)) =>
          Await((oa: Option[A]) => zip(feed(oa)(p1), recv2(oa)))
      }

    def feed[A,B](oa: Option[A])(p: Process[A,B]): Process[A,B] =
      p match {
        case Halt() => p
        case Emit(h,t) => Emit(h, feed(oa)(t))
        case Await(recv) => recv(oa)
      }

}



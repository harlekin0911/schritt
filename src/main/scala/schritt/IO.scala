package schritt

  /*
  The previous IO representation overflows the stack for some programs.
  The problem is that `run` call itself recursively, which means that
  an infinite or long running IO computation will have a chain of regular
  calls to `run`, eventually overflowing the stack.

  The general solution is to make the `IO` type into a data type that we
  interpret using a tail recursive loop, using pattern matching.
  */

sealed trait IO[A] {
    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f) // we do not interpret the `flatMap` here, just return it as a value
    def map[B](f: A => B): IO[B] = flatMap(f andThen (Return(_)))
}

case class Return[A](a: A) extends IO[A]
case class Suspend[A](resume: () => A) extends IO[A]
case class FlatMap[A,B](sub: IO[A], k: A => IO[B]) extends IO[B]

object IO extends /*Monad[IO]*/ {

    // Monad[IO]
    def unit[A](a: => A): IO[A] = Return(a)
    def flatMap[A,B](a: IO[A])(f: A => IO[B]): IO[B] = a flatMap f
    def suspend[A](a: => IO[A]) = Suspend(() => ()).flatMap { _ => a }

    def printLine(s: String): IO[Unit] = Suspend(() => Return(println(s)))
    val actions: Stream[IO[Unit]] = Stream.fill(100000)(printLine("Still going..."))
    val composite: IO[Unit]       = actions.foldLeft(IO.unit(())) { (acc, a) => FlatMap( acc, { a:Unit => IO.unit(a) }) }
    actions.foldLeft(IO.unit(())) { (acc, a) => acc flatMap { _ => a } }

    // There is only one sensible way to implement this as a
    // tail-recursive function, the one tricky case is left-nested
    // flatMaps, as in `((a flatMap f) flatMap g)`, which we
    // reassociate to the right as `a flatMap (ar => f(a) flatMap g)`
    @annotation.tailrec def run[A](io: IO[A]): A = io match {
        case Return(a) => a
        case Suspend(r) => r()
        case FlatMap(x, f) => x match {
             case Return(a) => run(f(a))
             case Suspend(r) => run(f(r()))
             case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
        }
    }
}

object IO2b {

  /*
   * As it turns out, there's nothing about this data type that is specific
   * to I/O, it's just a general purpose data type for optimizing tail calls.
   * Here it is, renamed to `TailRec`. This type is also sometimes called
   * `Trampoline`, because of the way interpreting it bounces back and forth
   * between the main `run` loop and the functions contained in the `TailRec`.
   */

  sealed trait TailRec[A] {
    def flatMap[B](f: A => TailRec[B]): TailRec[B] =
      FlatMap(this, f)
    def map[B](f: A => B): TailRec[B] =
      flatMap(f andThen (Return(_)))
  }
  case class Return[A](a: A) extends TailRec[A]
  case class Suspend[A](resume: () => A) extends TailRec[A]
  case class FlatMap[A,B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

  object TailRec /*extends Monad[TailRec]*/ {
    def unit[A](a: => A): TailRec[A] = Return(a)
    def flatMap[A,B](a: TailRec[A])(f: A => TailRec[B]): TailRec[B] = a flatMap f
    def suspend[A](a: => TailRec[A]) =
      Suspend(() => ()).flatMap { _ => a }

  }

  @annotation.tailrec def run[A](t: TailRec[A]): A = t match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }
}

object IO2bTests {
  import IO2b._

  val f: Int => TailRec[Int] = (i: Int) => IO2b.Return(i)

  val g: Int => TailRec[Int] =
    List.fill(10000)(f).foldLeft(f){
      (a: Function1[Int, TailRec[Int]],
        b: Function1[Int, TailRec[Int]]) => {
        (x: Int) => TailRec.suspend(a(x).flatMap(b))
      }
    }

  def main(args: Array[String]): Unit = {
    val gFortyTwo = g(42)
    println("g(42) = " + gFortyTwo)
    println("run(g(42)) = " + run(gFortyTwo))
  }
}


object runIO {
  def main( arg:Array[String]) : Unit = IO.run(IO.printLine("odel") );IO.run(IO.composite)
}

package fpis

object IOII {

  /*
   * As it turns out, there's nothing about this data type that is specific
   * to I/O, it's just a general purpose data type for optimizing tail calls.
   * Here it is, renamed to `TailRec`. This type is also sometimes called
   * `Trampoline`, because of the way interpreting it bounces back and forth
   * between the main `run` loop and the functions contained in the `TailRec`.
   */

  sealed trait TailRec[A] {
    def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)
    def map[B](f: A => B): TailRec[B] = flatMap(f andThen (Return(_)))
  }
  case class Return[A](a: A) extends TailRec[A]
  case class Suspend[A](resume: () => A) extends TailRec[A]
  case class FlatMap[A,B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

  object TailRec extends Monad[TailRec] {
    def unit[A](a: => A): TailRec[A] = Return(a)
    def flatMap[A,B](a: TailRec[A])(f: A => TailRec[B]): TailRec[B] = a flatMap f
    def apply[A](a: => A): TailRec[A] = unit(a)
  }

  @annotation.tailrec 
  def run[A](t: TailRec[A]): A = t match {
      case Return(a) => a
      case Suspend(r) => r()
      case FlatMap(x, f) => x match {
          case Return(a) => run(f(a))
          case Suspend(r) => run(f(r()))
          case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
      }
  }


    def main ( a : Array[String]) : Unit = {
    
        def f: Int => TailRec[Int] = (x: Int) => {Console.println( x+1);Return(x+1)}
        //val g = scala.List.fill(10000)(f).foldLeft(f) { (a, b) => x => Suspend(() =>  a(x).flatMap(b)) }
        def t( a: Int => TailRec[Int], b :Int => TailRec[Int]) = 
            (x :Int) => FlatMap( Suspend(() =>  a(x)),  (tx:TailRec[Int]) => tx.flatMap(b))
        val g = scala.List.fill(1000000)(f).foldLeft(f) (t) 
        //val g = scala.List.fill(1000000)(f).foldLeft(f) { (a, b) => x => FlatMap( Suspend(() =>  a(x)),  (x:TailRec[Int]) => x.flatMap(b)) }
        //val g = scala.List.fill(1000000)(f).foldLeft(f) { (a, b) => x => Suspend(() =>  a(x)).flatMap(  (x:TailRec[Int]) => x.flatMap(b)) }
        val x1 = g(0)
        val x2 = run(g(42))
        Console.println(x2)
    }
}

package schritt

sealed trait TailRec[A] {
    def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMapT(this, f)
    def map[B](f: A => B): TailRec[B] = flatMap(f andThen (ReturnT(_)))
}
case class ReturnT[A](a: A) extends TailRec[A]
case class SuspendT[A](resume: () => A) extends TailRec[A]
case class FlatMapT[A,B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

object TailRec {
  
    @annotation.tailrec def run[A](t: TailRec[A]): A = t match {
        case ReturnT(a) => a
        case SuspendT(r) => r()
        case FlatMapT(x, f) => x match {
             case ReturnT(a) => run(f(a))
             case SuspendT(r) => run(f(r()))
             case FlatMapT(y, g) => run( y flatMap (o => g(o) flatMap f))
    }
  }
    
  def suspend[A](a: => TailRec[A]) :TailRec[A] = SuspendT(() => ()).flatMap { _ => a }
}

object TailRecTests {

  val f: Int => TailRec[Int] = (i: Int) => ReturnT(i)

  val g: Int => TailRec[Int] = List.fill(10000)(f).foldLeft(f){
      (a: Function1[Int, TailRec[Int]], b: Function1[Int, TailRec[Int]]) => { (x: Int) => TailRec.suspend(a(x).flatMap(b)) }
  }

  def main(args: Array[String]): Unit = {
    val gFortyTwo = g(42)
    println("g(42) = " + gFortyTwo)
    println("run(g(42)) = " + TailRec.run(gFortyTwo))
  }
}
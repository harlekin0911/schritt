package schritt

import scala.collection.immutable.Range

sealed trait TailRec[A] {
    def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMapT(this, f)
    def map[B](f: A => B): TailRec[B] = flatMap(f andThen (ReturnT(_)))
}
case class ReturnT[A](a: A) extends TailRec[A]
case class SuspendT[A](resume: () => A) extends TailRec[A]
case class FlatMapT[A,B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

object TailRec {
  
    @annotation.tailrec 
    def run[A](t: TailRec[A]): A = { 
      //println( "Starte runprint");
      //print( t); 
      t match {
        case ReturnT(a) => a
        case SuspendT(r) => r()
        case FlatMapT(x, f) => /*run(f(run(x))) ist das selbe aber nicht tail recursive*/ x match {
             case ReturnT(a) => run(f(a))
             case SuspendT(r) => run(f(r()))
             //case FlatMapT(y, g) => run( y flatMap (o => g(o) flatMap f))
             //case FlatMapT(y, g) => run( FlatMapT(y, (o:Any) => g(o) flatMap f))
             case FlatMapT(y, g) => run( FlatMapT(y, (o:Any) => FlatMapT(g(o), f)))
        }
      }
    }
    
  @annotation.tailrec 
  def printR[A](t: TailRec[A]): Unit = t match {
        case ReturnT(a)     =>  println( "ReturnT(" + a + ")" )
        case SuspendT(r)    =>  println( "SuspendT(" + r + ")" )
        case FlatMapT(x, f) => { 
          //println( "FlatMapT(" + x + ", " + f + ")" )
          x match {
             case ReturnT(a) => println( a)
             case SuspendT(r) => printR( f(r()))
             case FlatMapT(y, g) => { /*printR(y);*/ printR( FlatMapT(y, (o:Any) => FlatMapT(g(o), f)))}
          }
           
          //println( "Ende FlatMapT(" + x + ", " + f + ")" )
        }
  }

  //def suspend[A](a: => TailRec[A]) :TailRec[A] = SuspendT(() => ()).flatMap { _ => a }
  def suspend[A](a: => TailRec[A]) :TailRec[A] = FlatMapT( SuspendT(() => ()), (y:Unit) => a )
}

object TailRecTests {

  val f: Int => TailRec[Int] = (i: Int) => ReturnT(i+1)

  val d = List.fill(10000)(f)
  val g: Int => TailRec[Int] = List.fill(10000000)(f).foldLeft(f){
      (a: Function1[Int, TailRec[Int]], b: Function1[Int, TailRec[Int]]) => { (x: Int) => TailRec.suspend(a(x).flatMap(b))} 
  }
  val h: Int => TailRec[Int] = Range(0, 4).toList.map(x => (i: Int) => ReturnT(i+1)).foldLeft(f){
      (a: Function1[Int, TailRec[Int]], b: Function1[Int, TailRec[Int]]) => { (x: Int) => TailRec.suspend(a(x).flatMap(b))} 
  }
  val i: Int => TailRec[Int] = Range(0, 5).toList.map(x => (i: Int) => { println( "f" + (5-x) + "(" + i + ")");ReturnT(i+1)}).foldLeft(f){
      (a: Function1[Int, TailRec[Int]], b: Function1[Int, TailRec[Int]]) => { (x: Int) => FlatMapT( SuspendT(() => ()), (y:Unit) => FlatMapT( a(x),b))} 
  }

  def main(args: Array[String]): Unit = {
    val gFortyTwo = g(42)
    TailRec.printR( gFortyTwo);
    println("run(g(42)) = " + TailRec.run(gFortyTwo))
  }
}
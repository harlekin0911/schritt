package functionalProgramming

class Something ( a:Int) {
  odel =>
}

import scala.language.higherKinds

trait Functor[F[_]] {
  def map[A,B](a: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](a: F[A])(f: A => F[B]): F[B]

  def map[A,B](a: F[A])(f: A => B): F[B] = flatMap(a)(a => unit(f(a)))

}

//sealed trait IO[A] { self =>
//    def run: A
////    def map[B]    (f: A => B)    : IO[B] = new IO[B] { def run = f(self.run) } // ohne Object
//    def map[B]    (f: A => B)    : IO[B] = IO(f(self.run) )
////    def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] { def run = f(self.run).run } // ohne Object
//    def flatMap[B](f: A => IO[B]): IO[B] =  IO[B]( f(self.run).run)
//    
//}

// Variante 2
sealed trait IO[A] {
    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
    def map[B](f: A => B): IO[B]         = flatMap(f andThen (Return(_)))
    
}

case class Return[A](a: A) extends IO[A]
case class Suspend[A](resume: () => A) extends IO[A]
case class FlatMap[A,B](sub: IO[A], k: A => IO[B]) extends IO[B]


// Ende Variante 2


object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = new IO[A] { def run = a }
    def flatMap[A,B](fa: IO[A])(f: A => IO[B]) = fa flatMap f
    def apply[A](a: => A): IO[A] = unit(a)

    def forever[A,B](a: IO[A]): IO[B] = {
       lazy val t: IO[B] = forever(a)
       a flatMap (_ => t)
    }
    
    @annotation.tailrec 
    final def  run[A](io: IO[A]): A = io match {
        case Return(a) => a
        case Suspend(r) => r()
        case FlatMap(x, f) => x match {
             case Return(a) => run(f(a))
             case Suspend(r) => run(f(r()))
             case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
        }
    }

}

object dada {
    def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0/9.0
    def ReadLine: IO[String] = new IO[String] { def run = readLine }
    def PrintLine(msg: String): IO[Unit] = new IO[Unit] { def run = println(msg) }
    def printLine(s: String): IO[Unit] = Suspend(() => Return(println(s)))
    def converter = {
// Variante 1
//          val a = PrintLine("Enter a temperature in degrees Fahrenheit: ").run
//          val b =  ReadLine.map(_.toDouble)
//          PrintLine(fahrenheitToCelsius(b.run).toString)
//    }

// Variante 2
         for {
            a <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
            d <- ReadLine.map[Double](_.toDouble)
            b <- PrintLine(fahrenheitToCelsius(d).toString)
        }  yield ()
        
// Variante 3
//       PrintLine("Enter a temperature in degrees Fahrenheit: ").flatMap( 
//           a => ReadLine.map( _.toDouble).flatMap(
//           b => PrintLine( fahrenheitToCelsius(b).toString)))
}
    
    
    def main ( a : Array[String]) : Unit = {
      //converter.run
      //println( odel)
      //val p = IO.forever(PrintLine("Still going... 1"))
      //p.run
      
      // Fehler Stack overflow
//      val f = (x:Int) => x
//      val g = scala.List.fill(30000)( f).foldLeft(f)(_ compose _)
//      g(42)
//      val i = 1
//      val j = scala.List.fill(100000)( i).foldLeft(i)(_ + _)
//      val h = j
//      Console.println( "H = " + h)
      
      // Funktioniert, kein overflow
      val pp = IO.forever(printLine("Still going... 2"))
      IO.run(pp)
    
      import tailrec._
          val h: Int => Int = x=> { Console.println( x+1);  x+1}
          def f: Int => TailRec[Int] = (x: Int) => tailrec.Return(x)
          //def t( a: Int => TailRec[Int], b :  Int => TailRec[Int]) : Int => TailRec[Int] =  (x:Int) =>  tailrec.Suspend(()=>h(x)).flatMap(b)
          //def t( a: Int => TailRec[Int], b :  Int => TailRec[Int]) =  (x :Int) => a(x).flatMap(b)
          //val g = scala.List.fill(100000)(f).foldLeft(f) ( t)
          //val g = scala.List.fill(100000)(f).foldLeft(f) { (a, b) => x => tailrec.Suspend(() =>  a(x).flatMap(b)) }
          //val x1 = TailRec.run(g(0))
          //val x2 = TailRec.run(g(42))
          //Console.println(x2)
    }
    
    def odel  = 
      for {
        a <- (1 to 5)
        b <- ( 2 to 7)
      } yield ( b)
      
}

object IO2b {


    sealed trait TailRec[A] {
        def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)
        def map[B]    (f: A => B):          TailRec[B] = flatMap(f andThen (Return(_)))
    }
    case class Return[A](a: A) extends TailRec[A]
    case class Suspend[A](resume: () => A) extends TailRec[A]
    case class FlatMap[A,B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

    object TailRec extends Monad[TailRec] {
        def unit[A](a: => A): TailRec[A] = Return(a)
        def flatMap[A,B](a: TailRec[A])(f: A => TailRec[B]): TailRec[B] = a flatMap f
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
}

package tailrec {

     sealed trait TailRec[A] {
        def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)
        def map[B]    (f: A => B)         : TailRec[B] = flatMap(f andThen (Return(_)))
     }

     case class Return[A](a: A) extends TailRec[A]
     case class Suspend[A](resume: () => A) extends TailRec[A]
     case class FlatMap[A,B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]
     
     object TailRec  extends Monad[TailRec] {
       
           def unit[A](a: => A): TailRec[A] = Return(a)
           def flatMap[A,B](a: TailRec[A])(f: A => TailRec[B]): TailRec[B] = a flatMap f

           @annotation.tailrec 
           final def  run[A](io: TailRec[A]): A = io match {
               case Return(a) => a
               case Suspend(r) => r()
               case FlatMap(x, f) => x match {
                   case Return(a)     => run(f(a))
                   case Suspend(r)    => run(f(r()))
                   case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
               }
           }
     }
}
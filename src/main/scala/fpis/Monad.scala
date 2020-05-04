package fpis

class Something ( a:Int) {
  odel =>
}

import scala.language.higherKinds

trait Functor[F[_]] {
  def map[A,B](a: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] { 
    self =>
    
    def unit[A](a: => A): F[A]
    def flatMap[A,B](a: F[A])(f: A => F[B]): F[B]

    def map[A,B](a: F[A])(f: A => B): F[B] = flatMap(a)(a => unit(f(a)))

    def map2[A,B,C](a: F[A], b: F[B])(f: (A,B) => C): F[C] = flatMap(a)(a => map(b)(b => f(a,b)))
  
    def sequence_[A](fs: Stream[F[A]]): F[Unit] = foreachM(fs)(skip)
  
    def sequence_[A](fs: F[A]*): F[Unit] = sequence_(fs.toStream)
  
    def replicateM[A](n: Int)(f: F[A]): F[List[A]] = List.fill(n)(f).foldRight(unit(List[A]()))(map2(_,_)(_ :: _))
  
    def replicateM_[A](n: Int)(f: F[A]): F[Unit] = foreachM(Stream.fill(n)(f))(skip)
  
    def as[A,B](a: F[A])(b: B): F[B] = map(a)(_ => b)
  
    def skip[A](a: F[A]): F[Unit] = as(a)(())
  
    def when[A](b: Boolean)(fa: => F[A]): F[Boolean] = if (b) as(fa)(true) else unit(false)
  
    def forever[A,B](a: F[A]): F[B] = {
        lazy val t: F[B] = a flatMap (_ => t)
        t
    }

    def foreverII[A,B](a: F[A]): F[B] = {
        lazy val t: F[B] = this.flatMap(a)(_ => t)
        t
    }

    def while_(a: F[Boolean])(b: F[Unit]): F[Unit] = {
        lazy val t: F[Unit] = while_(a)(b)
        a flatMap (c => skip(when(c)(t)))
    }
    def while_II(a: F[Boolean])(b: F[Unit]): F[Unit] = {
        lazy val t: F[Unit] = while_(a)(b)
        this.flatMap(a)(c => skip(when(c)(t)))
    }
    def doWhile[A](a: F[A])(cond: A => F[Boolean]): F[Unit] = for {
        a1 <- a
        ok <- cond(a1)
        _ <- if (ok) doWhile(a)(cond) else unit(())
    } yield ()

    def foldM[A,B](l: Stream[A])(z: B)(f: (B,A) => F[B]): F[B] =
        l match {
            case h #:: t => f(z,h) flatMap (z2 => foldM(t)(z2)(f))
            case _ => unit(z)
        }
 
    def foldM_[A,B](l: Stream[A])(z: B)(f: (B,A) => F[B]): F[Unit] = skip { foldM(l)(z)(f) }
  
    def foreachM[A](l: Stream[A])(f: A => F[Unit]): F[Unit] = foldM_(l)(())((u,a) => skip(f(a)))
  
    def seq[A,B,C](f: A => F[B])(g: B => F[C]): A => F[C] = f andThen (fb => flatMap(fb)(g))

    // syntax
    import scala.language.implicitConversions
    implicit def toMonadic[A](a: F[A]): Monadic[F,A] = new Monadic[F,A] { val F = self; def get = a }
}

trait Monadic[F[_],A] {
  
	val F: Monad[F]
    import F._
	def get: F[A]
	private val a = get
	def map[B](f: A => B): F[B] = F.map(a)(f)
	def flatMap[B](f: A => F[B]): F[B] = F.flatMap(a)(f)
	def **[B](b: F[B]) = F.map2(a,b)((_,_))
	def *>[B](b: F[B]) = F.map2(a,b)((_,b) => b)
	def map2[B,C](b: F[B])(f: (A,B) => C): F[C] = F.map2(a,b)(f)
	def as[B](b: B): F[B] = F.as(a)(b)
	def skip: F[Unit] = F.skip(a)
    def replicateM(n: Int) = F.replicateM(n)(a)
    def replicateM_(n: Int) = F.replicateM_(n)(a)
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

    override def forever[A,B](a: IO[A]): IO[B] = {
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
    def ReadLine: IO[String] = new IO[String] { def run = scala.io.StdIn.readLine }
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
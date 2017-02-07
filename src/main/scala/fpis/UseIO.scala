package fpis

object UseIO {
    trait UseIO { def run: Unit }
    def PrintLine(msg: String): UseIO = new UseIO { def run = println(msg) }
    def contest(p1: Player, p2: Player): UseIO = PrintLine(winnerMsg(winner(p1, p2)))
    
    def winnerMsg(p: Option[Player]): String = p map { case Player(name, _) => s"$name is the winner!"} getOrElse "It's a draw."
    //def contest(p1: Player, p2: Player): Unit = println(winnerMsg(winner(p1, p2)))

    case class Player(name: String, score: Int)
    //def contest(p1: Player, p2: Player): Unit = if (p1.score > p2.score)
    
    def winner(p1: Player, p2: Player): Option[Player] = 
        if (p1.score > p2.score) Some(p1)
        else if (p1.score < p2.score) Some(p2)
        else None
  
}

trait UseIO2 { self =>
    def run: Unit
    def ++(io: UseIO2): UseIO2 = new UseIO2 {
        def run = { self.run; io.run }
    }
}

object UseIO2 {
   def empty: UseIO2 = new UseIO2 { def run = () }
}

/*********************************************************************/

sealed trait UseIO3[A] { self =>
    def run: A
    def map[B](f: A => B): UseIO3[B] = new UseIO3[B] { def run = f(self.run) }
    def flatMap[B](f: A => UseIO3[B]): UseIO3[B] = new UseIO3[B] { def run = f(self.run).run }
}


object UseIO3 extends Monad[UseIO3] {
    def unit[A](a: => A): UseIO3[A] = new UseIO3[A] { def run = a }
    def flatMap[A,B](fa: UseIO3[A])(f: A => UseIO3[B]) = fa flatMap f
    def apply[A](a: => A): UseIO3[A] = unit(a)

    def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0/9.0
    
    def ReadLine: UseIO3[String] = UseIO3 { readLine }
    def PrintLine(msg: String): UseIO3[Unit] = UseIO3 { println(msg) }
    def converter: UseIO3[Unit] = for {
        _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
        d <- ReadLine.map(_.toDouble)
        _ <- PrintLine(fahrenheitToCelsius(d).toString)
    } yield ()
    
    def converter3: UseIO3[Unit] = 
        PrintLine("Enter a temperature in degrees Fahrenheit: ").
        flatMap( _ => ReadLine.map( _.toDouble ).
        flatMap( d => PrintLine(fahrenheitToCelsius(d).toString)).map( _=>()))
        
    val echo = ReadLine.flatMap(PrintLine)
    val readInt = ReadLine.map(_.toInt)
//    val readInts = readInt ** readInt
     replicateM(10)(ReadLine)
     
//     def factorial(n: Int): UseIO3[Int] = for {
//         acc   <- scala.Mutable(1)
//             _ <- foreachM (1 to n toStream) (i => acc.modify(_ * i).skip)
//        result <- acc.get
//    } yield result
//    
//    val factorialREPL: IO[Unit] = sequence_(
//         IO { println(helpstring) },
//         doWhile { IO { readLine } } { line =>
//             when (line != "q") { for {
//                 n <- factorial(line.toInt)
//                 _ <- IO { println("factorial: " + n) }
//         } yield () }
//     })
}
/*********************************************************************/
object IO4 {
  
    sealed trait IO4[A] {
        // we do not interpret the `flatMap` here, just return it as a value
        def flatMap[B](f: A => IO4[B]): IO4[B] = FlatMap(this, f) 
        def map[B](f: A => B): IO4[B] = flatMap(f andThen (Return(_)))
    }
  
    case class Return[A](a: A) extends IO4[A]
    case class Suspend[A](resume: () => A) extends IO4[A]
    case class FlatMap[A,B](sub: IO4[A], k: A => IO4[B]) extends IO4[B]

    object IO4 extends Monad[IO4] { // Notice that none of these operations DO anything
        def unit[A](a: => A): IO4[A] = Return(a)
        def flatMap[A,B](a: IO4[A])(f: A => IO4[B]): IO4[B] = /*FlatMap(a,f) */a flatMap f
        def suspend[A](a: => IO4[A]) = /*FlatMap(Suspend(()=>()),((_=>a):(Unit => IO4[A])))*/ Suspend(() => ()).flatMap { _ => a }
    }

    def printLine(s: String): IO4[Unit] = Suspend(() => Return(println(s)))

    val p = IO4.forever(printLine("Still going..."))

    val actions: Stream[IO4[Unit]] = Stream.fill(10000000)(printLine("Still going..."))
    val composite: IO4[Unit]       = actions.foldLeft(IO4.unit(())) { (acc, a) => acc flatMap { _ => a } }

    // There is only one sensible way to implement this as a
    // tail-recursive function, the one tricky case is left-nested
    // flatMaps, as in `((a flatMap f) flatMap g)`, which we
    // reassociate to the right as `a flatMap (ar => f(a) flatMap g)`
    @annotation.tailrec def run[A](io: IO4[A]): A = io match {
        case Return(a) => a
        case Suspend(r) => r()
        case FlatMap(x, f) => x match {
             case Return(a) => run(f(a))
             case Suspend(r) => run(f(r()))
             case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
        }
    }
    
    def main(args:Array[String]):Unit = run(p)
}


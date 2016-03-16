package fpis

sealed trait ProcessTest[I,O] {
    self =>
    
    import ProcessTest._

    def apply(s: Stream[I]): Stream[O] = this match {
        case Halt() => {Console.println( "Apply:Halt");Stream()}
        case Await(recv) => s match {
             case h #:: t => {Console.println( "Apply:Await");recv(Some(h))(t)}
             case xs => {Console.println( "Apply:Await(empty)");recv(None)(xs)} // Stream is empty
        }
        case Emit(h,t) => {Console.println( "Apply:Emit");h #:: t(s)}
    }

    def repeat: ProcessTest[I,O] = {
        def go(p: ProcessTest[I,O]): ProcessTest[I,O] = p match {
            case Halt() => { Console.println("repeat:halt");go(self)}
            case Await(recv) => { Console.println("repeat:await");Await { 
                 case None => recv(None)
                 case i => go(recv(i))}
            }
            case Emit(h, t) => { Console.println("repeat:emit");Emit(h, go(t))}
        }
        go(this)
    }
    
    def |>[O2](p2: ProcessTest[O,O2]): ProcessTest[I,O2] =  p2 match {
        case Halt() => Halt()
        case Emit(h,t) => Emit(h, this |> t)
        case Await(f) => this match {
            case Emit(h,t) => t |> f(Some(h))
            case Halt() => Halt() |> f(None)
            case Await(g) => Await((i: Option[I]) => g(i) |> p2)
        }
    }

}
  
object ProcessTest {

    case class Emit[I,O](   head: O, tail: ProcessTest[I,O] = Halt[I,O]()) extends ProcessTest[I,O]
    case class Await[I,O](  recv: Option[I] => ProcessTest[I,O]) extends ProcessTest[I,O]
    case class Halt[I,O]() extends ProcessTest[I,O]

    def emit[I,O](head: O, tail: ProcessTest[I,O] = Halt[I,O]()): ProcessTest[I,O] = Emit(head, tail)

    def await[I,O](f: I => ProcessTest[I,O], fallback: ProcessTest[I,O] = Halt[I,O]()): ProcessTest[I,O] =
        Await[I,O] {
            case Some(i) => f(i)
            case None => fallback
       }

    def liftOne[I,O](f: I => O): ProcessTest[I,O] = Await {
        case Some(i) => emit(f(i))
        case None => Halt()
    }

    def lift[I,O](f: I => O): ProcessTest[I,O] = liftOne(f).repeat
    
    def filter[I](p: I => Boolean): ProcessTest[I,I] = Await[I,I] {
        case Some(i) if p(i) => emit(i)
        case _ => Halt()
    }.repeat
    
    def sum: ProcessTest[Double,Double] = {
        def go(acc: Double): ProcessTest[Double,Double] = Await {
            case Some(d) => Emit(d+acc, go(d+acc))
            case None    => Halt()
        }
        go(0.0)
    }
    
    def take[I](n: Int): ProcessTest[I,I] = {
        def go(n:Int) : ProcessTest[I,I] = Await {
            case Some(d) if n> 0 => Emit(d, go(n-1))
            case _               => Halt()
        }
        go(n)
    }
    def takeII[I](n: Int): ProcessTest[I,I] =
        if (n <= 0) Halt()
        else await(i => emit(i, take[I](n-1)))
      
    def drop[I](n: Int): ProcessTest[I,I] =
        if (n <= 0) lift(i=>i)
        else await(i => drop[I](n-1))

    def takeWhile[I](f: I => Boolean): ProcessTest[I,I] = Await {
          case Some(i) if f(i) => Emit(i, takeWhile(f))
          case _       => Halt()
    }
    def dropWhile[I](f: I => Boolean): ProcessTest[I,I] = Await {
        case Some(i) => if ( f(i))  dropWhile(f) else Emit(i,lift(i=>i)) // Emit sonst fehlt das erste falsche Element
        case _       => Halt()
    }
    def dropWhileII[I](f: I => Boolean): ProcessTest[I,I] =
      await(i =>
        if (f(i)) dropWhileII(f)
        else      emit(i,lift(i=>i)))
        
    def count[I]: ProcessTest[I,Int] = {
        def go (n:Int) : ProcessTest[I,Int] = Await {
            case Some(d) => Emit(n+1, go(n+1))
            case _       => Halt()
        }
        go(0)
    }
    
    def mean: ProcessTest[Double,Double] = {
        def go (s:Double, c:Int) : ProcessTest[Double,Double] = Await {
            case Some(d) => Emit((s+d)/c+1, go(s+d,c+1))
            case _       => Halt()
        }
        go(0.0,0)
    }
        
    def loop[S,I,O](z: S)(f: (I,S) => (O,S)): ProcessTest[I,O] = await(
        (i: I) => f(i,z) match {
            case (o,s2) => emit(o, loop(s2)(f))
        })


}
object Test {
    import ProcessTest._
    def main( args : Array[String]) : Unit = {
        println(liftOne((x: Int) => x * 2)(Stream(1,2,3)).toList)
        println(lift(   (x: Int) => x * 2)(Stream(1,2,3)).toList)
        
//        val even = filter((x: Int) => x % 2 == 0)
//        val evens = even(Stream(1,2,3,4)).toList
//        Console.println( evens)
        
        println("dropWhile:   " + dropWhile( (x:Int) => x<3)(Stream(0,1,2,3,4)).toList)
        println("dropWhileII: " + dropWhileII( (x:Int) => x<3)(Stream(0,1,2,3,4)).toList)
        // Emit(1).repeat //Stack overflow
        println(take(6)(Stream(1,2,3,4,5)).toList)
        
        println( (filter((_:Int) % 2 == 0) |> lift(_ + 1)) (Stream(0,1,2,3,4)).toList)
          // runs forever
//        val units = Stream.continually(())
//        val ones = lift((_:Unit) => 1)(units)
//        println( ones.toList)
        
        
        println( (lift( (s:String) => s.toInt) |> lift((i:Int) => (i+1).toString))(Stream("1", "2", "3")).toList)
    }
}
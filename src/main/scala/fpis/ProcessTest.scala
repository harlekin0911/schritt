package fpis

sealed trait ProcessTest[I,O] {
    
    import ProcessTest._

    def apply(s: Stream[I]): Stream[O] = this match {
        case Halt() => Stream()
        case Await(recv) => s match {
             case h #:: t => recv(Some(h))(t)
             case xs => recv(None)(xs) // Stream is empty
        }
        case Emit(h,t) => h #:: t(s)
    }

    def repeat: ProcessTest[I,O] = {
        def go(p: ProcessTest[I,O]): ProcessTest[I,O] = p match {
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
}
object Test {
    import ProcessTest._
    def main( args : Array[String]) : Unit = {
        Console.println(liftOne((x: Int) => x * 2)(Stream(1,2,3)).toList)
        Console.println(lift(   (x: Int) => x * 2)(Stream(1,2,3)).toList)
    }
}
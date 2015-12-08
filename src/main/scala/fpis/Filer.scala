package fpis

object Filer {
    def linesGt40k(filename: String): IO[Boolean] = IO {
        val src = io.Source.fromFile(filename)
        try {
            var count = 0
            val lines: Iterator[String] = src.getLines
            while (count <= 40000 && lines.hasNext) {
                lines.next
                count += 1
            }
            count > 40000
        }
        finally src.close
    }
    
    val lines: Stream[String] = Stream()
    val js = lines.zipWithIndex.exists(_._2 + 1 >= 40000)
    
    def lines(filename: String): IO[Stream[String]] = IO {
        val src = io.Source.fromFile(filename)
        src.getLines.toStream append { src.close; Stream.empty }
    }
}

object dimpel {
    
  import Process._
    sealed trait Process[I,O] {
        def apply(s: Stream[I]): Stream[O] = this match {
            case Halt() => Stream()
            case Await(recv) => s match {
                 case h #:: t => recv(Some(h))(t)
                 case xs => recv(None)(xs)
            }
            case Emit(h,t) => h #:: t(s)
        }
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

    def liftOne[I,O](f: I => O): Process[I,O] = Await {
        case Some(i) => Emit(f(i))
        case None    => Halt()
    }

    object Process {
          case class Emit[I,O]( head: O, tail: Process[I,O] = Halt[I,O]()) extends Process[I,O]
          case class Await[I,O]( recv: Option[I] => Process[I,O])          extends Process[I,O]
          case class Halt[I,O]()                                           extends Process[I,O]
    }
    

    def main ( a : Array[String]) : Unit = {
        val p  = liftOne( (x:Int)=> x*2)
        val xs = p(Stream(1,2,3)).toList
         //Process( ("hans"::"peter"::Nil).toStream)
        Console.println( p)
        Console.println( xs)
    }
}




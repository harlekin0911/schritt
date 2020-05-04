package schritt

object peterle {
    val a : Stream[Double] = Stream.cons(1.00, Stream.cons( 2.0, a))
    val b : Stream[Double] = 1.00#::2.0#::a
    //a take 3
    
    def barwert( t:Int) : Stream[Double] = Stream.cons({Console.println("odel"); t.toDouble },barwert( t+1))
    def barwert2 : Stream[Double] = Stream.tabulate(20)(a=>a)
    
    def main(args: Array[String]) = {
      
      import scala.language.postfixOps
      
       Console.println( a take 5 toList)
       Console.println( b take 5 toList)
       Console.println( a.drop( 5) take 1 )
       Console.println( barwert(5).head)
       Console.println( barwert(1).head)
       Console.println( barwert(5).head)
       Console.println( barwert2.take( -1).head)
  }

}


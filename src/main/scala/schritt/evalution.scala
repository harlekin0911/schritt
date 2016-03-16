package schritt

object evalution {

   def loop: Int = {println("loop");loop}
   def first( x: Int, y: Int) = x
   def constOne( x: Int, y: => Int) = 1
  
  def main( args : Array[String]) = {
     //Console.println( constOne( 1, loop)) // stops after first call
     Console.println( first( 1, loop))      // runs forever
  }
     
}
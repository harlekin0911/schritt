package schritt

object evalution {

  def loop: Int = loop
  
  def first( x: Int, y: Int) = x
  
  def main( args : Array[String]) = {
     Console.println( constOne( 1, loop))
     Console.println( first( 1, loop))
  }
     
   def constOne( x: Int, y: => Int) = 1
}
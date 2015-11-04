package schritt

object curry {

//  def sum( f: Int => Int) : ( Int, Int) => Int = {
//    def sumF( a: Int, b: Int): Int = 
//      if ( a > b) 0 else f(a) + sumF(a + 1, b)
//    sumF
//  }
  
  def sum( f: Int => Int)( a: Int, b: Int): Int =
    if ( a > b) 0 else f(a) + sum(f)(a + 1, b)

//  def sumInts = sum( x => x)(1,5) 
//  def sumSquares = sum ( x=> x * x)
//  def sumPowersOfTwo = sum( powerOfTwo)
  
  def powerOfTwo( x: Int) : Int = if ( x == 0) 1 else 2 * powerOfTwo( x -1)
  
  def main(args: Array[String]) = {
    Console.println( sum( x => x)    (1,5))
    Console.println( sum( x => x*x)  (1,5))
    Console.println( sum( powerOfTwo)(1,5))
  }

}
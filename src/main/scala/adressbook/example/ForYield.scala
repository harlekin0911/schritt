package example

/** Turn command line arguments to uppercase */
object ForYield {
  def main(args: Array[String]) {
    val name = "Paul"
    println( name)
    val res = for (a <- args) yield a.toUpperCase
    for ( s <- res) println("Arguments: " + s.toString)
    println("Arguments: " + res)
    Console.println( res)
    
  }
}
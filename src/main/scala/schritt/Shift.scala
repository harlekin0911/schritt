package schritt

object Shift {
  
  def main ( args : Array[String]) : Unit = println( Range(1,100).map( x => (x >>2)).mkString("\n"))
}
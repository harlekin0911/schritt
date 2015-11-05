package enums

object Status extends NamedEnumeration {
  val aufrecht =  NamedVal( name = "haha", number = 7, text = "", description = "" ) // with NamedEnumeration
  val beendet  =  NamedVal( "Beendet", 1, "was aeiss ich", "halt so")
}

object Odel extends Enumeration {
  
  val jauche = Value
  val hans = Value
}

object test {
  
 // import 
  
  val alle = Status.values
  
  def main(args: Array[String]) = {
    println( alle)
     println (Odel.apply(1))
    // import Odel._
     Odel.values foreach( d => println( d.id + " " + d))
     Status.values foreach( d => println( d.id + " " + d))
    Status.values filter (d => d.id == 7) foreach ( d => println( d))
    
    println( Status.aufrecht)
    println( Status.parse( 7))
    println( Status.parse( 3))
    println( Status.parse( "haha"))
  }
      

}

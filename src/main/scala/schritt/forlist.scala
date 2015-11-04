package schritt

object forlist {
    val a = 1::2::3::4::5::Nil
    
    def main(args: Array[String]) = {
      
    val a = 1::2::3::4::5::Nil
    
    // Kreuzprodukt
    val b = for ( i<- a; j<-a.drop(1)) yield (i,j)
       
    Console.println( b)
       
     Console.println(a.zip( a.drop(1)))
  }

}


package schritt

 object barred   {
  
  
  def main(args: Array[String]) = {
    val a = (1, 0.5)::( 2, 0.5)::(3, 0.5)::Nil
    
    a.reduceLeft((a,b) => ( a._1 + b._1, a._2 + b._2))
    Console.println( a.foldLeft(0.0,1.0)((a,b)=> ( a._1 + b._1, b._1* a._2 *( 1- b._2))))
    Console.println( (1-0.5)*(1-0.5) *(1-0.5))
  }
}


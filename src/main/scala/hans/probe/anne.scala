package probe


trait element {
  val parent   : element
  val children : Map[String, String]
  def getChildren : Map[String,String] = if ( this.parent == elementBase) children else children ++ parent.getChildren
}

object elementBase extends element {
  
  val parent     = elementBase
  val children   = Map.empty[String,String]
}
//class anne  

object anne extends element {
    val parent = elementBase
    val children  : Map[String,String]  =   Map( "sophie" -> "a" , "Markus" -> "")
}


//class anneMarie extends anne 
object anneMarie extends element  {
    val parent = anne
    val children : Map[String,String]  = Map("odel" -> "" ) 
}

object anneMarieMaiere extends element {
    val parent = anneMarie
    val children : Map[String,String]  = Map( "Lena" -> "" ) 
}

object run {

  def main(args: Array[String]) = {
      
      val a = anneMarie; 
    	  Console.println( a.getChildren)
    	  Console.println( a.children)
      val b = anne; 
    	  Console.println( b.getChildren)
    	  Console.println( b.children)
    	  
    	  Console.println( anneMarieMaiere.getChildren)
    	  Console.println( anneMarieMaiere.children)
  }
  
}
package probe

abstract class base { 
    def childs = List.empty[String]
    val kind : List[String]
}

class hans extends childs {
    override val kind = List( "sophie", "Markus")
}

class hansPeter extends hans with childs {
    override val kind = List( "odel")
    
    //def getSuperChild : List[String] = super.kind
}

trait childs extends base {
	
    
    val kind = List.empty[String]

    override def  childs : List[String] = {
        var a = List[String]()
        a ++= super.childs
        a ++= kind
        a
    }
}

object hansPeter {

  def main(args: Array[String]) = {
      
      val a = new hansPeter(); 
    	  Console.println( a.childs)
    	  Console.println( a.childs)
    	  Console.println("a" )
    	  //Console.println( a.getSuperChild )
      val b = new hans(); 
    	  Console.println( b.childs)
    	  
      val c: hans = a
    	  Console.println( c.childs)
  }

}
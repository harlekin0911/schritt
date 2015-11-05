package enums


abstract class ID extends Enumeration {
  
    def parse( n : String) : Option[Value] = {
     val l = this.values filter (d => d.toString == n) 
     if ( l.isEmpty) None
     else Some( l.head)
  }

}

class AttributeID extends ID
class EntityID    extends ID

class daddel {
  
  import enums.impl.AttributeID
  val d  : Map[ AttributeID.Value, String] = Map( AttributeID.hans -> "", AttributeID.anne -> "")
}

abstract class dudel {
  import enums.impl.AttributeID
  val a : AttributeID#Value = AttributeID.hans
  
  def haha : AttributeID#Value
}
object testa extends dudel {
  
  import enums.impl.AttributeID
  override def haha = AttributeID.hans
     
  def main(args: Array[String]) = {
       
       val d = new daddel
       
       
       println( d)
  
       import enums.impl.EntityID
       println( EntityID.parse( "peter"))
       println ( haha)
     }

}
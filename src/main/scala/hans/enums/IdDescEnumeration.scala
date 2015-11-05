package enums

import scala.language.implicitConversions

class IdDescEnumeration extends Enumeration {
  
  case class IdDescVal(i: Int, name: String) extends Val
  
  implicit def valueToIdDescVal(v: Value): IdDescVal = v.asInstanceOf[IdDescVal]
  
  def parse( n : Int) : Option[IdDescVal] = {
     val l = this.values filter (d => d.id == n) 
     if ( l.isEmpty) None
     else Some( l.head)
  }
}

class IdAttributeDescEnumeration extends IdDescEnumeration {
  
  case class IdAttributeDescVal(i: Int, name: String) extends Val
  
  implicit def valueToIdAttributeDescVal(v: Value): IdAttributeDescVal = v.asInstanceOf[IdAttributeDescVal]
  implicit def IdDescValToIdAttributeDescVal(v: Value): IdAttributeDescVal = v.asInstanceOf[IdAttributeDescVal]
  
}

class IdEntityDescEnumeration extends IdDescEnumeration {
  
  case class IdEntityDescVal(i: Int, name: String) extends Val
  
  implicit def valueToIdAttributeDescVal    (v: Value): IdEntityDescVal = v.asInstanceOf[IdEntityDescVal]
  implicit def IdDescValToIdAttributeDescVal(v: Value): IdEntityDescVal = v.asInstanceOf[IdEntityDescVal]
  
}

object EntityName extends  IdEntityDescEnumeration {
  val hans  = IdEntityDescVal;
  val peter = IdEntityDescVal;
}

object run {
  
//  def diel[T <: Enumeration](name:String, enum:T):T#Value =    
//    enum.valueOf(name) match {
//      case Some(x) => x
//      case x => throw new RuntimeException("No field named '" + name + "' found on enum " + enum + ", legal values = " + enum.values)
//    }

  
   //val bla : IdEntityDescEnumeration#Value = EntityName.peter
   //def dada : Map[ String, IdEntityDescEnumeration#Value] = Map( "diri" -> EntityName.peter)
   def main(args: Array[String]) = {
     
     val a = Map( EntityName.peter -> "bla", EntityName.hans-> "HaHA")
     
     println( a)
   }
   
}
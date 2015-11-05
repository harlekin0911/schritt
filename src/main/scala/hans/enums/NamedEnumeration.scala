package enums


class NamedEnumeration extends Enumeration {
  
  case class NamedVal( val name: String, val number: Int, val text: String, val description: String) extends Val( number, name) 
  import scala.language.implicitConversions

  implicit def valueToNamedValue(v: Value): NamedVal = v.asInstanceOf[NamedVal]
  
  def parse( n : Int) : Option[NamedVal] = {
     val l = this.values filter (d => d.id == n) 
     if ( l.isEmpty) None
     else Some( l.head)
  }

  def parse( n : String) : Option[NamedVal] = {
     val l = this.values filter (d => d.name == n) 
     if ( l.isEmpty) None
     else Some( l.head)
  }
}
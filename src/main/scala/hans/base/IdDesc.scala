package src.base

abstract class IdDesc extends Enumeration {
  
    def parse( n : String) : Option[Value] = {
     val l = this.values filter (d => d.toString == n) 
     if ( l.isEmpty) None
     else Some( l.head)
  }

}

class IdAttribute   extends IdDesc
class IdEntity      extends IdDesc
class IdDatatype    extends IdDesc
class IdEnumeration extends IdDesc


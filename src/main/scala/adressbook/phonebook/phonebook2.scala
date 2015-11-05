/* examples/xml/phonebook/phonebook2.scala */
package phonebook;

import scala.xml.{ UnprefixedAttribute, Elem, Node, Null, Text, TopScope, PrettyPrinter } 

object phonebook2 {

  import scala.xml.Node

  /** adds an entry to a phonebook */
  def add( p: Node, newEntry: Node ): Node = p match {

      case <phonebook>{ ch @ _* }</phonebook> => 

        <phonebook>{ ch }{ newEntry }</phonebook>
  }

  def add2( p: Node, e: Node ) = Elem(null, p.label, Null, TopScope, (p.child ++ e):_*)

  val pb2_2 = <entry>
           <name>Kim</name> 
           <phone where="work">+41 21 111 11 11</phone>
         </entry> 
  val pb2 = 
    add( phonebook1.labPhoneBook, 
         <entry>
           <name>Kim</name> 
           <phone where="work">+41 21 111 11 11</phone>
         </entry> );

  def main( args: Array[String] ) = {
    Console.println( pb2 )
    Console.println( add2( phonebook1.labPhoneBook, pb2_2))
  }
}


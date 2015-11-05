/* examples/phonebook/embeddedBook.scala */
package phonebook  

import scala.xml.{ UnprefixedAttribute, Elem, Node, Null, Text, TopScope, PrettyPrinter } 

object embeddedBook {

  val company  = <a href="http://acme.org">ACME</a>
  val first    = "Burak"
  val last     = "Emir"
  val location = "work"

  val embBook = 
    <phonebook>
      <descr>
        This is the <b>phonebook</b> of the 
        {company} corporation.
      </descr>
      <entry>
        <name>{ first+" "+last }</name> 
        <phone where={ location }>+41 21 693 68 {val x = 60 + 7; x}</phone>
      </entry>
    </phonebook>;

    val z = if (true)  { Some(Text("pizza")) } else { None }
    val y = if (false) { Some(Text("pizza")) } else { None }
    <foo bar={z}>{ /*lots of code*/ }</foo>

  def main(args: Array[String]) = {
    Console.println( <foo bar={z}>{ /*lots of code*/ }</foo> )
    Console.println( <foo bar={y}>{ /*lots of code*/ }</foo> )
    Console.println( embBook )
  }

}


/* examples/xml/phonebook/verboseBook.scala */
package phonebook 

object verboseBook {

  import scala.xml.{ UnprefixedAttribute, Elem, Node, Null, Text, TopScope, PrettyPrinter } 

  val pbookVerbose = 
    Elem(null, "phonebook", Null, TopScope, true,
       Elem(null, "descr", Null, TopScope, true,
            Text("This is a "), 
            Elem(null, "b", Null, TopScope, true,Text("sample")),
            Text("description")
          ),
       Elem(null, "entry", Null, TopScope,true,
            Elem(null, "name", Null, TopScope, true, Text("Burak Emir")),
            Elem(null, "phone", new UnprefixedAttribute("where","work", Null), TopScope, true,
                 Text("+41 21 693 68 67"))
          )
       )

//  def main(args: Array[String]) = 
//    Console.println( pbookVerbose )
    
  def main(args: Array[String]) = 
    Console.println( new PrettyPrinter(80 /*width*/,3 /*indent*/).format(pbookVerbose) )

}


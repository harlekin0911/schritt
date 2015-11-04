package schritt


object MyImplicit {
  
  object Foo {
	      implicit def b : Other.type = Other
        trait Other
			  object Other {
	          implicit val a = 3
			      def findAnInt(implicit x : Int) = x
        }
  }
  
  import Foo.Other.a
  def main(args: Array[String]) : Unit = {
    Console.println(implicitly[MyImplicit.Foo.Other.type])
    Console.println( Foo.Other.findAnInt)
  }

}
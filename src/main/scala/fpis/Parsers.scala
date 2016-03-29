package fpis

/**
 * Chapter 8
 */

//object Parsers {
//    
//    def main (args:Array[String]) : Unit = {
//        
//        val p : Parsers[Unit,List] = {
//            def run[A]( p:List[A])(input:String) :Either[Unit,A] = input.find(_ == 'c') match {
//                case None => Left(None)
//                case c    => Right(c.get)
//            } 
//            def char(c: Char): Parser[Char] = 
//            
//            
//        }
//        p.run(char('c'))('c'.toString) == Right('c')
//    }
//}

trait Parsers[ParseError, Parser[+_]] { 
    self =>
	def run[A](p: Parser[A])(input: String): Either[ParseError,A]

	def char(c: Char): Parser[Char]
    
	def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
    
	implicit def string(s: String): Parser[String]
    
	implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
    
	implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
    
    ParserOps[String] = ParserOps(f(a))
    
    case class ParserOps[A](p: Parser[A]) {
         def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
         def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    }
	
	def map[A,B](a: Parser[A])(f: A => B): Parser[B]
	
	def many[A](p: Parser[A]): Parser[List[A]]
	
//	import fpinscala.testing._
//	object Laws {
//         def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
//         forAll(in)(s => run(p1)(s) == run(p2)(s))
//         def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
//         equal(p, p.map(a => a))(in)
//}
}

object StringP {
    
//    def sp[Parser[+_]]( p:Parsers[Unit,Parser]) : Parser[String] = {
//        1
//        
//    }
}
object map2 {
   def map2 [A,B,C](a:List[A], b:List[B])(f:(A,B) => C): List[C] = for { x <- a; y <- b } yield f(x,y)
   def map2a[A,B,C](a:List[A], b:List[B])(f:(A,B) => C): List[C] = a.flatMap( x => b.map( y => f(x,y)))
}
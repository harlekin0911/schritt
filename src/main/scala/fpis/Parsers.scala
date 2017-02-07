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
trait ParseError

import scala.language.implicitConversions
import scala.language.higherKinds

import Either._

trait Parsers[Parser[+_]] { 
    self =>
	def run[A](p: Parser[A])(input: String): Either[ParseError,A]

	def char(c: Char): Parser[Char] = string( (c.toString)).map(_.charAt(0))
    
	def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
    
	implicit def string(s: String): Parser[String]
    
	implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
    
	implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
    
    case class ParserOps[A](p: Parser[A]) {
         def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
         def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
         def map[B](f: A => B): Parser[B] = self.map(p)(f)
	     def product[B]( p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
	     def **     [B]( p2: Parser[B]): Parser[(A,B)] = product(p2)
	     def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    }
	
	def map[A,B](a: Parser[A])(f: A => B): Parser[B] = a.flatMap( x => succeed(f(x)))
	def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)]  =  p.flatMap( x => p2.map( y => (x,y)))
	def slice[A](p:Parser[A]): A
	def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

    def map2[A,B,C] (p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C]  = product(p,p2).map(f.tupled)
    def map2a[A,B,C] (p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] = p.flatMap( a => p2.map( b => f( a,b)))

	def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)
	def many[A](p: Parser[A]): Parser[List[A]]  = map2(p, many(p))(_ :: _) or succeed(List())
	
	def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = 
        if (n <= 0) succeed(List())
        else map2(p, listOfN(n-1, p))(_ :: _)
        
	def succeed[A](a: A): Parser[A] = string("") map (_ => a)
	//def succeed[A](a:A): Parser[A] = a => Success(a, 0)
	
	
	
//	import fpinscala.testing._
//	object Laws {
//         def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
//         forAll(in)(s => run(p1)(s) == run(p2)(s))
//         def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
//         equal(p, p.map(a => a))(in)
//}
}

//object StringP {
//    
//    def sp[Parser[+_]]( p:Parsers[Parser]) : Parser[String] = {
//        
//        val p : Parser[String] = Parser[String] {
//            def ac
//        }
//        p
//        
//    }
//}
object map2 {
   def map2 [A,B,C](a:List[A], b:List[B])(f:(A,B) => C): List[C] = for { x <- a; y <- b } yield f(x,y)
   def map2a[A,B,C](a:List[A], b:List[B])(f:(A,B) => C): List[C] = a.flatMap( x => b.map( y => f(x,y)))
   
   def product[A,B]( a:List[A], b:List[B]) : List[(A,B)] = for { x <- a; y <- b } yield (x,y)
}
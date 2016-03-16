package schritt

import scala.reflect.ClassTag

trait CC[_]

class Variance[B] {
  
  def foo(x : List[_ >: Int]) = x
  
  val doed = foo(List("Hi"))
  
  // View-Bounds
  def foo[A <% B](x: A) = x
  def foo1[A](x: A)(implicit  $ev :( A => B)) = x
  def foo2[A](x: A)(implicit  f :( A => B)) = x
  
  // Context-Bounds
  def foo[A : CC](x: A) = x
  def foo1[A](x: A)(implicit $ev0: CC[A]) = x

}

object V {
  def first[T](x : Traversable[T]) = (x.head, x)
  val a = first(Array(1,2))
  //def first[A: ClassManifest](x : Array[A])  = Array(x(0))
  def first1[A: ClassTag](x : Array[A])  = Array(x(0))
  
  def peek[A, C <: Traversable[A]](col : C) = (col.head, col)
  // Val c kann nur mit der folgenden Definition zugewiesen werden, ansonsten Compiler Fehler
  def peek[C, A](col: C)(implicit ev: C <:< Traversable[A]) = (col.head, col)
  val c = peek(List(1,2,3))
}

object aa {
  def ff( i:Int) = i
}

// default and absract function
abstract class aa( val a:Int, val b:Int =2, val c:Int) {
  def f ( a:Int) : Int
}

// const
class b extends aa( 1,3,2) {
   def f(a:Int) :Int  = schritt.aa.ff(a)
}
class c extends b {
  override def f(a:Int) : Int = 4
}

object tt {
  val s = schritt.aa.ff(_)
  val rr = new b;
}
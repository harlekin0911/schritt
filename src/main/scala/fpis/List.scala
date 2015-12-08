package fpis

sealed trait ListFp[+A]

case object NilFp extends ListFp[Nothing]
case class Cons[+A](head: A, tail: ListFp[A]) extends ListFp[A]

object ListFp {

    def sum(ints: ListFp[Int]): Int = ints match {
        case NilFp => 0
        case Cons(x,xs) => x + sum(xs)
    }

    //@annotation.tailrec
    def product(ds: ListFp[Double]): Double = ds match {
        case NilFp => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x,xs) => x * product(xs)
    }

    def apply[A](as: A*): ListFp[A] =
        if (as.isEmpty) NilFp
        else Cons(as.head, apply(as.tail: _*))
        
    @annotation.tailrec
    def dropWhile[A](l: ListFp[A], f: A => Boolean): ListFp[A] = {
        l match {
        case Cons(h,t) if f(h) => dropWhile(t,f)
        case _ => l
        }
    }
    
    @annotation.tailrec
    def dropWhileII[A](as: ListFp[A])(f: A => Boolean): ListFp[A] =
        as match {
        case Cons(h,t)  if f(h)  => dropWhileII(t)(f)
                       //else Cons(h,dropWhileII(t)(f))
        case _ => as
        }
    
    @annotation.tailrec
    def foldLeft[A,B](as: ListFp[A], z: B)(f: (B, A) => B): B = as match {
        case NilFp => z
        case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
    }
    
    //@annotation.tailrec
    def foldRight[A,B](as: ListFp[A], z: B)(f: (A, B) => B): B = as match {
        case NilFp => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
    
    def sum2(ns: ListFp[Int]) = foldRight(ns, 0)((x,y) => x + y)

    def product2(ns: ListFp[Double]) = foldRight(ns, 1.0)(_ * _)
    
    def length[A](as: ListFp[A]): Int = foldRight( as, 0)( (_,c) => c+1)
    def length2[A](as: ListFp[A]): Int = foldLeft( as, 0)( (c,_) => c+1)
    

  def foldRightViaFoldLeft  [A,B](l: ListFp[A], z: B)(f: (A,B) => B): B =  foldLeft(reverse(l), z)((b,a) => f(a,b))
  //def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =  foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)
  def foldRightViaFoldLeft_1[A,B](l: ListFp[A], z: B)(f: (A,B) => B): B =  foldLeft(l, (b:B) => b)((g : B => B,a :A) => (b => g(f(a,b))))(z)
  
  def foldLeftViaFoldRight[A,B](l: ListFp[A], z: B)(f: (B,A) => B): B =   foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  //def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc,h) => Cons(h,acc))
  def reverse[A](l: ListFp[A]): ListFp[A] = foldLeft(l, ListFp[A]())((acc : ListFp[A],h : A) => Cons(h,acc))

}

object run  {
    def main ( a : Array[String]) : Unit = {

        val xs: ListFp[Int] = ListFp(1,2,3,4,5,2,1)
        val ex1 = ListFp.dropWhile(xs, (x: Int) => x < 4)
        Console.println(ex1)
        
        val xsa: ListFp[Int] =  ListFp(1,2,3,4,5,2,1)
        val ex1a = ListFp.dropWhileII(xsa)(x => x < 4)
        Console.println(ex1a)

        val x  = ListFp(1,2,3,4,5) match {
            case Cons(x, Cons(2, Cons(4, _))) => x
            case NilFp => 42
            case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
            case Cons(h, t) => h + ListFp.sum(t)
            case _ => 101
         }
         Console.println(x)
         
         val c = ListFp.foldRight(ListFp(1,2,3), NilFp:ListFp[Int])(Cons(_,_))

    }
}

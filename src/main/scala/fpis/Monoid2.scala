package fpis

trait Monoid2[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid2 {
  
   val intAddition: Monoid2[Int] = new Monoid2[Int] {
        def op(x: Int, y: Int) = x + y
        val zero = 0
    }

    import scala.collection.immutable.List
    def foldMap[A, B](as: List[A], m: Monoid2[B])(f: A => B): B =
            as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  
    def odel[A] ( A : Monoid2[A])(a1: A, a2: A) = A.op( a1,a2)
    def odela[A] ( A : Monoid2[A]) = A.op _
  
    val s = odel[Int] (intAddition) _
    
    def mapMergeMonoid[K,V](V: Monoid2[V]): Monoid2[Map[K, V]] =
    new Monoid2[Map[K, V]] {
        def zero = Map[K,V]()
        def op(a: Map[K, V], b: Map[K, V]) =
            (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
             acc.updated(k, V.op(a.getOrElse(k, V.zero),
                              b.getOrElse(k, V.zero)))
        }
    }

    def a[A] : A => Int = A => 1
    
    val b = a[Monoid2[Int]](intAddition)
    
    val M: Monoid2[Map[String, Map[String, Int]]] =  mapMergeMonoid(mapMergeMonoid(intAddition))
    val m1 = Map("o1" -> Map("i1" -> 1, "i2" -> 2))
    val m2 = Map("o1" -> Map("i2" -> 3))
    val m3 = M.op(m1, m2)
    
  
}
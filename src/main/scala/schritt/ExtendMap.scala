package schritt

class ExtendMap[E,I>:Int]( m:Map[E,I])  extends Map[E,I] {
    
    def get( e:E) : Option[I] = m.get(e)
    def -(e:E) : ExtendMap[E,I] = new ExtendMap( m - e)
    def iterator : Iterator[(E,I)] = m.iterator
    def +[B1 >:I](kv:(E,B1)) : ExtendMap[E,B1] = new ExtendMap(m + kv)
}

class ExtendMap2[E]( m:Map[E,Int])  extends Map[E,Int] {
    
    def get( e:E) : Option[Int] = m.get(e)
    def -(e:E) : ExtendMap2[E] = new ExtendMap2( m - e)
    def iterator : Iterator[(E,Int)] = m.iterator
    def +[B1 >:Int](kv:(E,B1)) : ExtendMap2[E] = kv._2 match {
        case a:Int => new ExtendMap2(m + ((kv._1,a)))
        case _     => throw new RuntimeException( "not allowed")
    }
}

object ExtendMap {
    
    def apply[E] = new ExtendMap( Map.empty:Map[E,Int])
    
    def main(args:Array[String]) : Unit = {
        
        val a = ExtendMap[String]
        val b = a + (( "hans", 3))
        val c = b + (("peter", "bla")) 
        val d = c + (("ursel", 2.0)) 
        println( d)

        val a2 = new ExtendMap2[String]( Map.empty)
        val b2 = a2 + (( "hans", 3))
        val c2 = b2 + (("peter", "bla")) 
        println( c2)
    }
}
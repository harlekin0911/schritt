package schritt

class ExtendMap[E,I>:Int]( m:Map[E,I])  extends Map[E,I] {
    
    def get( e:E) : Option[I] = m.get(e)
    def -(e:E) : ExtendMap[E,I] = new ExtendMap( m - e)
    def iterator : Iterator[(E,I)] = m.iterator
    //def +[B1 >:I](kv:(E,B1)) : Map[E,B1] = m + kv
    def +[B1 >:I](kv:(E,B1)) : ExtendMap[E,B1] = new ExtendMap(m + kv)
}

object ExtendMap {
    
    def apply[E] = new ExtendMap( Map.empty:Map[E,Int])
    
    def main(args:Array[String]) : Unit = {
        
        val a = ExtendMap[String]
        val b = a + (( "hans", 3))
    }
}
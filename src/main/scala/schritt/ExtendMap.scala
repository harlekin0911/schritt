package schritt

import scala.collection.immutable.MapLike
import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom
import scala.collection.IndexedSeqLike
import scala.collection.mutable.ArrayBuffer
import scala.collection.GenMap
import scala.collection.generic.MapFactory

class ExtendMap[E,I>:Int] private ( m:Map[E,I])  
    //extends Map[E,I] 
    //with scala.collection.Map[E, I]
    //extends MapLike[E, I,Map[E,I]]
    extends IndexedSeq[(E,I)]
    with IndexedSeqLike[(E,I), ExtendMap[E,I]]
{

    //def seq: scala.collection.Map[E,I] = ???      
    //override def empty: Map[E,I] = ExtendMap.empty 

    override def apply(idx: Int): (E, I) = m.toSeq(idx)   
    override def length: Int = m.size
    
    def get( e:E) : Option[I] = m.get(e)
    def -(e:E) : ExtendMap[E,I] = new ExtendMap( m - e)
    override def iterator : Iterator[(E,I)] = m.iterator
    def +[B1 >:I](kv:(E,B1)) : ExtendMap[E,B1] = new ExtendMap(m + kv)
    
    override protected[this] def newBuilder: Builder[(E,I), ExtendMap[E,I]] = ExtendMap.newBuilder
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

object ExtendMap /* extends MapFactory[ExtendMap] */ {
    
    def apply[E,I>:Int] : ExtendMap[E,I] = new ExtendMap( Map.empty:Map[E,I])
    def apply[E,I>:Int]( m:Map[E,I]) = new ExtendMap( m)
    
    def empty[E,I>:Int] = new ExtendMap( Map.empty:Map[E,I])//ExtendMap.apply
    
    def newBuilder[E,I>:Int]: Builder[(E,I), ExtendMap[E,I]] = new ArrayBuffer[(E,I)].mapResult( (buf:Seq[(E,I)]) => ExtendMap(buf.toMap))
        
    implicit def canBuildFrom[E,I>:Int]: CanBuildFrom[ExtendMap[E,I], (E,I), ExtendMap[E,I]] = 
        new CanBuildFrom[ExtendMap[E,I], (E,I), ExtendMap[E,I]] {
            def apply : Builder[(E,I), ExtendMap[E,I]] = newBuilder
            def apply(from: ExtendMap[E,I]): Builder[(E,I), ExtendMap[E,I]] = newBuilder
        }
}

object runExtendsMap {
    
    def main(args:Array[String]) : Unit = {
        
        val a = ExtendMap[String,Int]
        val b = a + (( "hans", 3))
        val c = b + (("peter", "bla")) 
        val d = c + (("ursel", 2.0)) 
        println( d)

        val a3 = new ExtendMap2[String]( Map.empty)
        val b3 = a3 + (( "hans", 3))
        val c3 = b3 + (("peter", 5)) 
        println( c3)

        val a2 = new ExtendMap2[String]( Map.empty)
        val b2 = a2 + (( "hans", 3))
        val c2 = b2 + (("peter", "bla")) 
        println( c2)
        
        val m = ExtendMap( Map( "a" -> 1))
        val mc = m.map(x => (x._1, x._2 *2) )
        val md = m.take(1)
        val me = m.find( x => x._2 == 3)
    }
}
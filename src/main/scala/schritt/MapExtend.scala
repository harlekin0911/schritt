package schritt

import scala.collection.MapLike

import scala.collection.mutable.{Builder, MapBuilder}

class MapExtend[T] private( m: Map[String,T]) extends Map[String, T] with MapLike[String, T, MapExtend[T]] {
    
    override def get(key: String): Option[T] = m.get(key)
    override def iterator: Iterator[(String, T)] = m.iterator
    override def + [T1 >: T](kv: (String, T1)) = new MapExtend(m + kv)
    override def -(key: String) = new MapExtend( m - key)
    
    override def empty = new MapExtend[T]( Map.empty)
    
    //override protected[this] def newBuilder: Builder[(String,T), MapExtend[T]] = newBuilder // mapResult fromSeq 
}

import scala.collection.generic.CanBuildFrom

object MapExtend extends {
    
    def empty[T] = new MapExtend[T]( Map.empty)
    
    def apply[T](kvs: (String, T)*): MapExtend[T] = {
        val m: MapExtend[T] = empty
        for (kv <- kvs)
            m + kv
        m
    }
    
    def newBuilder[T]: Builder[(String, T), MapExtend[T]] = new MapBuilder[String, T, MapExtend[T]](empty)
    
    implicit def canBuildFrom[T] : CanBuildFrom[MapExtend[_], (String, T), MapExtend[T]] = 
        new CanBuildFrom[MapExtend[_], (String, T), MapExtend[T]] {
            def apply(from: MapExtend[_]) = newBuilder[T]
            def apply() = newBuilder[T]
    }
}
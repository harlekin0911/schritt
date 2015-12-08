package schritt

class ReduceByKey {
  
    def reduceByKey[K,V](collection: Traversable[Tuple2[K, V]])(implicit num: Numeric[V]) = {    
        import num._
        collection.groupBy(_._1).map {case (group, traversable) => traversable.reduce{(a,b) => (a._1, a._2 + b._2)}}
    }
}
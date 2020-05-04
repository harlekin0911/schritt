package schritt

import scala.collection.mutable.ArrayBuffer

class Matrix(private val repr : Array[Array[Double]]) {
    def row(idx : Int) : Seq[Double] = {
        repr(idx)
    }
    def col(idx : Int) : Seq[Double] = {
      import scala.language.postfixOps
        repr.foldLeft(ArrayBuffer[Double]()) { 
          (buffer, currentRow) => buffer.append(currentRow(idx)) 
          buffer } toArray
        }
    lazy val rowRank = repr.size
    lazy val colRank = if(rowRank > 0) repr(0).size else 0
    override def toString = "Matrix" + repr.foldLeft("") { (msg, row) => msg + row.mkString("\n|", " | ", "|") }
}

trait ThreadStrategy {
    def execute[A](func : Function0[A]) : Function0[A]
}

object MatrixUtils {
    def multiply(a: Matrix, b: Matrix)( implicit threading : ThreadStrategy = SameThreadStrategy): Matrix = {
        assert(a.colRank == b.rowRank)
        val buffer = new Array[Array[Double]](a.rowRank)
        for ( i <- 0 until a.rowRank ) {
            buffer(i) = new Array[Double](b.colRank)
        }
        
        def computeValue(row : Int, col : Int) : Unit = {
            val pairwiseElements = a.row(row).zip(b.col(col))
            val products = for((x,y) <- pairwiseElements)
                yield x*y
            val result = products.sum
            buffer(row)(col) = result
        }
    
        val computations = for {
            i <- 0 until a.rowRank
            j <- 0 until b.colRank
        } yield threading.execute { () => computeValue(i,j) }
        
        computations.foreach(_())
        new Matrix(buffer)

    }
}

object SameThreadStrategy extends ThreadStrategy {
    def execute[A](func : Function0[A]) = func
}

import java.util.concurrent.{Callable, Executors}
object ThreadPoolStrategy extends ThreadStrategy {
    val pool = Executors.newFixedThreadPool( java.lang.Runtime.getRuntime.availableProcessors)
    def execute[A](func : Function0[A] ) = {
        val future = pool.submit(new Callable[A] {
            def call() : A = {
                Console.println("Executing function on thread: " + Thread.currentThread.getName)
                func()
            }
         })
         () => future.get()
    }
}

object Matrix {

  def main ( args: Array[String]) : Unit = {
    val x = new Matrix(Array(Array(1,2,3), Array(4,5,6)))
    val y = new Matrix(Array(Array(1), Array(1), Array(1)))
    
    Console.println( MatrixUtils.multiply(x,y))
  }
}


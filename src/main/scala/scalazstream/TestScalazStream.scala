package scalazstream

import scalaz.stream._
import scalaz.concurrent.Task

object TestScalazStream {

	val converter: Task[Unit] = io.linesR("testdata/fahrenheit.txt")
			.filter(s => !s.trim.isEmpty && !s.startsWith("//"))
			.map(line => fahrenheitToCelsius(line.toDouble).toString)
			.intersperse("\n")
			.pipe(text.utf8Encode)
			.to(io.fileChunkW("testdata/celsius.txt"))
			.run

    // at the end of the universe...
    val u: Unit = converter.run
			
	def fahrenheitToCelsius(d:Double) = d*2 
	
	//val p : Process[Int,Int] = Process(2) //((i:Int) => i*2)
	
	val j = Process(2)
	
	import scalaz.stream.process1
	val k = process1.lift { (_:Int) => 1 }
	
	def main( args:Array[String]) = {
	    u
	}
}
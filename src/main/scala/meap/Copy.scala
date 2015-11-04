package meap

 class Copy ( val a : Int, val b : Int) {
     def copy( a : Int = this.a, b : Int = this.b) = new Copy( a, b )
}

case class CopyC ( val c : Int, override val a :Int, override val  b:Int) extends Copy(a,b)

object Copy {
	
	def main(args: Array[String]) : Unit = {
	  
	  val vv = new Copy( 1,2)
	  val bb = vv.copy( b = 1)
	}
}

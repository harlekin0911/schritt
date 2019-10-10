package file

    import scala.io.Source


object ReadFile {
  
	def main( args : Array[String]) : Unit = {

	  //val filename = "J:/users/p001117/home/run/baNreMonitorZzr/BaNReMonitorZzrBestand-20190919-152958.csv"
	  val filename = "J:/users/p001117/home/run/baNreMonitorZzr/BaNReMonitorZzrBestand-20190923-154347.csv"
		val d = Source.fromFile(filename).getLines.foldRight(0.0)( sum)
		  
		println( "Die Summe ist " + f"$d%1.2f" )
  }
	
	def sum( l: String, xd: Double) : Double = {
		val ds = l.split(";")(34)

		try {
				val d = ds.toDouble
				xd + d
		} catch {
				case _ : Throwable  => println( "exception"); xd
		}
	}
}
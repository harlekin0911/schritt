package file

import scala.io.Source
import scala.io.Codec


object ReadFile {

	def main( args : Array[String]) : Unit = {

	  
	  //val bestand   = "J:/users/p001117/home/run/baNreMonitorZzr/ergebnisse/20190930/BaNReMonitorZzrBestand-20190930-165440.csv"
	  //val summen    = "J:/users/p001117/home/run/baNreMonitorZzr/ergebnisse/20190930/BaNReMonitorZzrSummen-20190930-165440.csv"
	  
			//val bestand   = "J:/users/p001117/home/run/baNreMonitorZzr/ergebnisse/20191010/BaNReMonitorZzrBestand-20191010-150210.csv"
			//val summen    = "J:/users/p001117/home/run/baNreMonitorZzr/ergebnisse/20191010/BaNReMonitorZzrSummen-20191010-150210.csv"
	  
	   val bestand    = "J:/users/p001117/home/run/baNreMonitorZzr/BaNReMonitorZzrBestand-20191014-94124.csv"
	   val summen    = "J:/users/p001117/home/run/baNreMonitorZzr/BaNReMonitorZzrSummen-20191014-94124.csv"

			val d = Source.fromFile(bestand)(Codec.ISO8859).getLines
			val e = d.foldRight( ( 0.0, 0.0, 0.0))( sumbestand)

			val f = Source.fromFile(summen)(Codec.ISO8859).getLines
			val g = f.foldRight( ( 0.0, 0.0, 0.0))( sumSumme)

			println( "Die Summen im Bestand sind aktJahresrente:<" + "%1.2f".format( e._1) + "> aktVSTod:<  " +  "%1.2f".format(e._2) + "> aktRDR:<  " + "%1.2f".format( e._3)  +">")
			println( "Die Summen in der Sum sind aktJahresrente:<" + "%1.2f".format( g._1) + "> aktVSTod:<  " +  "%1.2f".format(g._3) + "> aktRDR:<  " + "%1.2f".format( g._2)  +">")
	}

	def sumbestand( l: String, xd: (Double,Double,Double)) : (Double,Double,Double) = {
			try {

				val ds = l.split(";")
						( xd._1 + ds(32).toDouble, xd._2 + ds(33).toDouble,  xd._3 + ds(34).toDouble)
			} catch {
			case _ : Throwable  => println( "exception"); xd
			case _ : java.nio.charset.MalformedInputException  => println( "exception"); xd
			}
	}

	def sumSumme( l: String, xd: (Double,Double,Double)) : (Double,Double,Double) = {
			try {

				val ds = l.split(";")
						( xd._1 + ds(4).toDouble, xd._2 + ds(5).toDouble,  xd._3 + ds(6).toDouble)
			} catch {
			case _ : Throwable  => println( "exception"); xd
			case _ : java.nio.charset.MalformedInputException  => println( "exception"); xd
			}
	}

}
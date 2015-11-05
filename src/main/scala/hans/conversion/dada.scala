package conversion


object dada {

    import scala.language.implicitConversions

    implicit def doubleToInt(x: Double) = x.toInt
}

class dada {
  import dada.doubleToInt
  val i : Int = 3.5
}

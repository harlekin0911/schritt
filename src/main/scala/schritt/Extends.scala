package schritt


sealed class Sex 

object Sex {
    object male   extends Sex
    object female extends Sex
}

trait bar[A] { 
    def qx( t: Int) : Double 
    def apply( t:Int) : Double =qx(t)
}

object bar {
    
    def apply(  q: Int => Double)               = new bar[      Int  => Double] { def qx(t:Int) : Double = q(t)} 
    def apply( sq: (Sex, Int) => Double)(s:Sex) = new bar[(Sex, Int) => Double] { def qx(t:Int) : Double = sq(s,t)}
}
  
object jawohl {
    def main(args: Array[String]) = {
        Console.println( bar(   _=> 1.0)(3))
        Console.println( bar( (t:Int)=> t.toDouble)(3))
        Console.println( bar( (s:Sex,t:Int) => 3.0)(Sex.male)(3))
    }
}
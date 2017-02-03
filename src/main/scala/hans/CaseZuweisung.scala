package hans

object CaseZuweisung {
    
    case class C(i:Int,d:Double)
    
    val  C(x,y) = C(3,3.0)
  
}
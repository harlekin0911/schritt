package schritt

//object CaseInheritanceAbstract {
  
    abstract class abase {
        def a :String
        def copy(a:String = this.a) : abase
    }
    case class cbase(a:String) extends abase {
        def copy(a:String = this.a) = cbase(a)
    }
 
//    abstract class second extends base {
//        def b:Double
//        def copy(a:String=this.a, b:Double=this.b) : second
//    }
//    case class csecond(a:String,b:Double) extends second {
//        def copy(a:String=this.a, b:Double=this.b) = csecond(a,b)
//        def copy(a:String=this.a) = csecond(a,this.b)
//    }
//    abstract class third extends second {
//        def a = "hans"
//        def c : Int
//        def copy(b:Double=this.b, c:Int = this.c) : third
//    }
//    case class cthird(b:Double,c:Int) extends third {
//        def copy(b:Double=this.b) = cthird(b,c)
//    }
//}
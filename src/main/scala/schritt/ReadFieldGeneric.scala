package schritt

import scala.util.Failure
import scala.util.Success
import scala.util.Try

class ReadFieldGeneric {
  
    def getAttribute(  name : String) : Try[String] = {
       val f = getCCParams( this)
       if ( f.contains( name)) Success(getCCParams( this)(name).toString) 
       else                    Failure(new RuntimeException( " Der Name <" + name + "> ist nicht erlaubt an <" + toString + ">")) 
    }

    def getCCParams(cc: AnyRef) : Map[String,Any] =
        (Map[String, Any]() /: cc.getClass.getDeclaredFields) {
          (a, f) => f.setAccessible(true) 
          a + (f.getName -> f.get(cc))
         }

}
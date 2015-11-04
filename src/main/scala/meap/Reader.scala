package meap

import java.util.Date
import scala.util.Try


  case class Reader[R, A](run: R => A) {
     def map[B](f: A => B): Reader[R, B] = Reader(r => f(run(r)))
     def flatMap[B](f: A => Reader[R, B]): Reader[R, B] = Reader(r => f(run(r)).run(r))
  }
class AccountRepository

trait AccountServiceI[Account, Amount, Balance] { 
   def open(no: String, name: String, openingDate: Option[Date]): Reader[AccountRepository, Try[Account]]
   def close(no: String, closeDate: Option[Date]): Reader[AccountRepository, Try[Account]]
   def debit(no: String, amount: Amount):  Reader[AccountRepository, Try[Account]]
   def credit(no: String, amount: Amount): Reader[AccountRepository, Try[Account]]
   def balance(no: String): Reader[AccountRepository, Try[Balance]]
}

//object App extends AccountService [String, BigDecimal,Double] {
//    def op(no: String) = for {
//        _  <- credit(no, BigDecimal(100))
//        _  <- credit(no, BigDecimal(300))
//        _  <- debit(no, BigDecimal(160))
//        b <- balance(no)
//    } yield b
//    
//    def main(args: Array[String]) : Unit = {
//      
//      Console.println( op("hans"))
//    }
//}
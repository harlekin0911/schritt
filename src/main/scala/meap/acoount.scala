package meap

import java.util.Date
import java.math.BigDecimal

import scala.util.Try
import scala.util.Failure
import scala.util.Success

import scala.language.implicitConversions


case class Amount ( val a: Double = 0.0) {
  implicit def apply( d : Double) = new Amount( d) 
  implicit def apply( i : Int)    = new Amount( i) 
  def < ( am : Amount) = a < am.a 
  def - ( am : Amount) = Amount(a - am.a)
  def + ( am : Amount) = Amount(a + am.a)
}

case class Balance(amount: Amount = Amount(0.0))

case class Account( no: String, name: String, dateOfOpening: Date, balance: Balance = Balance()) 

trait AccountService { 

    def debit(a: Account, amount: Amount): Try[Account] = {
         if (a.balance.amount < amount)
             Failure(new Exception("Insufficient balance in account"))
         else Success(a.copy(balance = Balance((a.balance.amount - amount))))
    }

    def credit(a: Account, amount: Amount) = Success(a.copy(balance = Balance(a.balance.amount + amount)))
    
}

object AccountService extends AccountService

import AccountService._

object accrun {
   import AccountService._

   val date = new Date( 1,1,2015)
   val a = Account("a1", "John", date)
   a.balance == Balance(Amount(0))
   val b = credit(a, Amount(1000))

   val c = debit(b.value, Amount(200))
   
   val f = Account("a1", "John", date) 
   credit(f, Amount( 100.0)).flatMap(debit(_, Amount(100.0)))
   
}

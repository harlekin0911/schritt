import java.util.{ Date, Calendar }
import util.{ Try, Success, Failure }

case class Balance[Amount](amount: Amount = 0)

sealed trait Account { 
    type Amount = BigDecimal
    def no: String
    def name: String
    def dateOfOpen: Option[Date]
    def dateOfClose: Option[Date]
    def balance: Balance[Amount]
}

final case class CheckingAccount //private[domain] 
    (no: String, name: String, dateOfOpen: Option[Date], dateOfClose: Option[Date] = None, balance: Balance[BigDecimal] = Balance()) extends Account 

final case class SavingsAccount //private[domain] 
    (no: String, name: String, rateOfInterest: BigDecimal, dateOfOpen: Option[Date], dateOfClose: Option[Date] = None, balance: Balance[BigDecimal] = Balance())
extends Account 

object Account {

    type Amount = BigDecimal
    def today = Calendar.getInstance.getTime

    def checkingAccount(no: String, name: String, openDate: Option[Date], closeDate: Option[Date], balance: Balance[Amount]): Try[Account] = { 
        val cd = closeDate.getOrElse(today)
        val od = openDate.getOrElse(today)
        if (cd before od)
            Failure(new Exception(s"Close date [$cd] cannot be earlier than open date [$od]"))
        else Success(CheckingAccount(no, name, openDate, closeDate, balance))
    }
    
    def savingsAccount(no: String, name: String, rate: BigDecimal,openDate: Option[Date], closeDate: Option[Date], balance: Balance[Amount]): Try[Account] = { 
        val cd = closeDate.getOrElse(today)
        val od = openDate.getOrElse(today)
        if (cd before od)
             Failure(new Exception(s"Close date [$cd] cannot be earlier than open date [$od]") )
        else if (rate <= BigDecimal(0))
             Failure(new Exception(s"Interest rate $rate must be > 0"))
        else Success(SavingsAccount(no, name, rate, openDate, closeDate, balance))
    }
}
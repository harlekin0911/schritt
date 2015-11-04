package meap

import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global
object UseFuture {
  
  def calculateInterest[A](account: A):Future[BigDecimal] = { 
    if (0== 0) throw new Exception("Interest Rate not found")
    else Future(BigDecimal(10000))
    }

  val s = "Hello"
  val f: Future[String] = Future { s + " future!"}
  
}
  import concurrent.Promise
case class TaxCut(reduction: Int)

object Government {

    // either give the type as a type parameter to the factory method:
    val taxcut = Promise[TaxCut]()
    // or give the compiler a hint by specifying the type of your val:
    val taxcut2: Promise[TaxCut] = Promise()

    val taxcutF: Future[TaxCut] = taxcut.future


    def redeemCampaignPledge(): Future[TaxCut] = {
        val p = Promise[TaxCut]()
    Future {
      println("Starting the new legislative period.")
      Thread.sleep(2000)
      p.success(TaxCut(20))
      println("We reduced the taxes! You must reelect us!!!!1111")
    }
    p.future
  }
}
  
sealed trait DayOfWeek { 
    val value: Int
    override def toString = value match {
         case 1 => "Monday"
         case 2 => "Tuesday"
         case 3 => "Wednesday"
         case 4 => "Thursday"
         case 5 => "Friday"
         case 6 => "Saturday"
         case 7 => "Sunday"
    }
}
object DayOfWeek { 
    private def unsafeDayOfWeek(d: Int) = new DayOfWeek { val value = d }
    private val isValid: Int => Boolean = { i => i >= 1 && i <= 7 }
    def dayOfWeek(d: Int): Option[DayOfWeek] = if (isValid(d)) Some(unsafeDayOfWeek(d)) else None 
    def apply( d: Int) = dayOfWeek( d)
}
object bla {
  def a = new DayOfWeek { val value = 1}
  def b = DayOfWeek( 1)
  
  def s = a == b
}
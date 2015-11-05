import java.time.LocalDate
import java.time.temporal.ChronoUnit

object DateTest {

  def main(args: Array[String]) = {
    
    val l = LocalDate.of( 1965, 4,6)
    val h = LocalDate.now()
    val d = ChronoUnit.MONTHS.between(l, h)

    Console.println(l + h.toString() + " " +  d)
    

  }
}
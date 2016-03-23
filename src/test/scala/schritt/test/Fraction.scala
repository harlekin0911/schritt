package schritt.test

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.Matchers
import org.scalatest.FunSuite
import org.scalatest.Suite



class TestFraction  extends FunSuite with GeneratorDrivenPropertyChecks with Matchers { // with PropertyChecks {

    test ("odel") {
	    forAll { (n: Int, d: Int) =>

	        whenever (d != 0 && d != Integer.MIN_VALUE && n != Integer.MIN_VALUE) {


		        val f = new schritt.test.Fraction(n, d)

		        if (n < 0 && d < 0 || n > 0 && d > 0)
			        f.numer should be > 0
			    else if (n != 0)
				    f.numer should be < 0
			    else
			        f.numer should be === 0
			        f.denom should be > 0

	        }
	   }
    }
}



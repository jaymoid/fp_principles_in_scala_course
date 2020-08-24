package recfun

import org.junit.Assert.assertEquals
import org.junit._

class RecFunSuite {

  import RecFun._

  // ------ balance tests -----------------------------------------------------

  @Test def `balance: '(if (zero? x) max (/ 1 x))' is balanced`: Unit =
    assert(balance("(if (zero? x) max (/ 1 x))".toList))

  @Test def `balance: 'I told him ...' is balanced`: Unit =
    assert(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))

  @Test def `balance: ':-)' is unbalanced`: Unit =
    assert(!balance(":-)".toList))

  @Test def `balance: counting is not enough`: Unit =
    assert(!balance("())(".toList))


  // ------ countChange tests -------------------------------------------------
  @Test def `countChange: base cases`: Unit = {
    assertEquals(1, countChange(0, List(1, 2)))
    assertEquals(0, countChange(40, List()))
  }

  @Test def `countChange: example given in instructions`: Unit =
    assertEquals(3, countChange(4, List(1, 2)))

  @Test def `countChange: sorted CHF`: Unit =
    assertEquals(1022, countChange(300, List(5, 10, 20, 50, 100, 200, 500)))

  @Test def `countChange: no pennies`: Unit =
    assertEquals(0, countChange(301, List(5, 10, 20, 50, 100, 200, 500)))

  @Test def `countChange: unsorted CHF`: Unit =
    assertEquals(1022, countChange(300, List(500, 5, 50, 100, 20, 200, 10)))

  @Test def `countChange: requested money is not necessarily a multiple of smallest denomination` = {
    assertEquals(1, countChange(7, List(2, 5)))
  }

  // Count change tests...
  @Test def `countChangeCombinations: base cases`: Unit = {
    assertEquals(List(List()), changeCombinations(0, List(1, 2))) // one combination of 0 coins = 0 money
    assertEquals(List(), changeCombinations(40, List())) // no possible combinations
  }

  @Test def `countChangeCombinations: simple cases, 1 coin`: Unit = {
    assertEquals(List(List(1)), changeCombinations(1, List(1)))
    assertEquals(List(List(1, 1)), changeCombinations(2, List(1)))
    assertEquals(List(List(1, 1, 1)), changeCombinations(3, List(1)))
  }

  @Test def `countChangeCombinations: simple cases, 2 coin`: Unit = {
    val coins = List(1, 2)
    assertEquals(List(List(1)), changeCombinations(1, coins))
    assertEquals(List(List(2), List(1, 1)), changeCombinations(2, coins))
    assertEquals(List(List(2, 1), List(1, 1, 1)), changeCombinations(3, coins))
    assertEquals(List(List(2, 2), List(2, 1, 1), List(1, 1, 1, 1)), changeCombinations(4, coins))
    assertEquals(List(List(2, 2, 1), List(2, 1, 1, 1), List(1, 1, 1, 1, 1)), changeCombinations(5, coins))
  }

  @Test def `countChangeCombinations: simple cases, 3 coin`: Unit = {
    val coins = List(1, 2, 5)
    assertEquals(List(List(1)), changeCombinations(1, coins))
    assertEquals(List(List(2), List(1, 1)), changeCombinations(2, coins))
    assertEquals(List(List(2, 1), List(1, 1, 1)), changeCombinations(3, coins))
    assertEquals(List(List(2, 2), List(2, 1, 1), List(1, 1, 1, 1)), changeCombinations(4, coins))
    assertEquals(List(List(5), List(2, 2, 1), List(2, 1, 1, 1), List(1, 1, 1, 1, 1)), changeCombinations(5, coins))
  }

  @Test def `countChangeCombinations: no compatible way of making combo`: Unit = {
    assertEquals(List(), changeCombinations(301, List(5, 10, 20, 50, 100, 200, 500)))
  }

  @Test def `countChangeCombinations: requested money is not necessarily a multiple of smallest denomination` = {
    assertEquals(List(List(5,2)), changeCombinations(7, List(2, 5)))
  }

  // ------ pascal tests ------------------------------------------------------

  @Test def `pascal: col=0,row=0`: Unit = assertEquals(1, pascal(0, 0))

  @Test def `pascal: col=0,row=1`: Unit = assertEquals(1, pascal(0, 1))

  @Test def `pascal: col=0,row=2`: Unit = assertEquals(1, pascal(0, 2))

  @Test def `pascal: col=1,row=1`: Unit = assertEquals(1, pascal(1, 1))

  @Test def `pascal: col=1,row=2`: Unit = assertEquals(2, pascal(1, 2))

  @Test def `pascal: col=2,row=2`: Unit = assertEquals(1, pascal(2, 2))

  @Test def `pascal: col=1,row=3`: Unit = assertEquals(3, pascal(1, 3))

  @Test def `pascal: last row  1 4 6 4 1`: Unit = {
    assertEquals(1, pascal(0, 4))
    assertEquals(4, pascal(1, 4))
    assertEquals(6, pascal(2, 4))
    assertEquals(4, pascal(3, 4))
    assertEquals(1, pascal(4, 4))
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}

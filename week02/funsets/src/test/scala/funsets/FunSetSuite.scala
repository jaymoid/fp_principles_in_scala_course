package funsets

import org.junit.Assert.{assertFalse, assertTrue}
import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * @Ignore annotation.
   */
  @Test def `singleton set one contains one`: Unit = {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `intersect contains only elements in both sets`: Unit = {
    new TestSets {
      val bigSet = union(union(s1, s2), s3) // set 1,2,3
      val anotherSet = union(s3, singletonSet(4)) // set 3,4
      val s = intersect(bigSet, anotherSet)
      assert(contains(s, 3), "Interset 3")
      assert(!contains(s, 4), "Interset 2")
      assert(!contains(s, 2), "Interset 3")
    }
  }

 @Test def `forAll checks returns true for all in bounds`: Unit = {
   val setOf10_000 = { n: Int => n > 0 && n<= 10_000}
   val notNegative = { n:Int => n >= 0 }
   assertTrue(forall(setOf10_000,notNegative))
 }

  @Test def `forAll checks returns false and true appropriately`: Unit = {
    val setOfAllEven = { n: Int => n % 2 == 0 }
    val isEven = { n:Int =>n % 2 == 0}
    val setOfAllOddsAndSix = { n: Int => n % 2 == 1 || n == 6}

    assertTrue(forall(setOfAllEven, isEven))
    assertFalse(forall(setOfAllOddsAndSix, isEven)) // 6 isnt even
  }

  @Test def `thereExists checks returns false and true appropriately`: Unit = {
    val setOfAllOdds = { n: Int => n % 2 == 1 }
    val isEven = { n:Int =>n % 2 == 0}
    val setOfAllOddsAndSix = { n: Int => n % 2 == 1 || n == 6}

    assertTrue(exists(setOfAllOddsAndSix, isEven))
    assertFalse(forall(setOfAllOdds, isEven)) // 6 isnt even
  }

  @Test def `map transforms elements in the set`: Unit = {
    val newSet :FunSet = map(singletonSet(1), {n => n+1})
    assertTrue(contains(newSet, 2))
    assertFalse(contains(newSet, 1))
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}

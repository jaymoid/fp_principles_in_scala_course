package funsets

import scala.annotation.tailrec

/**
 * 2. Purely Functional Sets.
 */
trait FunSets extends FunSetsInterface {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  override type FunSet = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: FunSet, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): FunSet = { n =>
    n == elem
  }

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: FunSet, t: FunSet): FunSet = { n =>
    s(n) || t(n)
  }

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: FunSet, t: FunSet): FunSet = { n =>
    s(n) && t(n)
  }

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: FunSet, t: FunSet): FunSet = { n =>
    s(n) && !t(n)
  }

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: FunSet, p: Int => Boolean): FunSet = { n =>
    s(n) && p(n)
  }


  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: FunSet, p: Int => Boolean): Boolean = {
    @tailrec
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (s(a) && !p(a)) false
      else iter(a + 1)
    }

    iter(-bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: FunSet, p: Int => Boolean): Boolean = {
    !forall(s, { n => !p(n) })
  }

  // Example
  // set1 = Set[1,2,3,4,5]
  // predIs3 = x == 3
  // (forAll set1 predIs3) == false // all the set is not equal to 3
  // (forAll set1 !predIs3) ==  false // all of the set is not not equal to 3
  //                                  // or, forall of the set, e != 3 is false
  // therfore
  // exists = !(forAll set !pred)

  // Another eg. same set
  // predLT10 = x < 10
  // (forAll set1 predLT10) == true
  // (forAll set1 !predLT10) == false // (forAll [1,2,3,4,5]  e >= 10) == false
  // exists = !(forAll set1 !predLT10)

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: FunSet, f: Int => Int): FunSet = { queryElement =>
    exists(s, { origElement => queryElement == f(origElement) })
  }
  // thinking about set as a function, where true indicates the query element is in the set
  // setA = {q => q == 5 }
  // f = {n => n * 10}
  // mappedSet =  {q =>
  //          exists(setA, {e => f(e) == q }
  // }

  /**
   * Displays the contents of a set
   */
  def toString(s: FunSet): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: FunSet): Unit = {
    println(toString(s))
  }
}

object FunSets extends FunSets

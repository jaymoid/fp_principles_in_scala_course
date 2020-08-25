package week02

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import week02.Sum._

class SumTest extends AnyFunSuite {

  val identity = { x: Int => x }

  test("add numbers in range"){
    sum(identity, 1, 10) should be(55)
  }

  val squared = { x: Int => x * x }

  test("return the squared of numbers in range") {
    sum(squared, 1, 10) should be(385)
  }

}

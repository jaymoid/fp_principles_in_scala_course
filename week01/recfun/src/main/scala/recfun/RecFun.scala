package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    (c, r) match {
      case (x, y) if y < x || x < 0 => 0
      case (0, 0) | (0, 1) | (1, 1) => 1
      case _ => pascal(c - 1, r - 1) + pascal(c, r - 1)
    }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def countOpened(xs: List[Char], count: Int = 0): Boolean =
      if (xs.isEmpty) count == 0
      else if (count < 0) false
      else countOpened(xs.tail, if (xs.head == '(') count + 1 else count - 1)

    countOpened(chars.filter { x => x == '(' || x == ')' })
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) {
      1
    } else if (coins.isEmpty) {
      0
    } else {
      val maxCoin = coins.max
      val coinsMinusMaxCoin = coins.filterNot { x => x == maxCoin }
      val rem = money - maxCoin
      val combosWithMaxCoin = countChange(rem, if (rem >= maxCoin) coins else coinsMinusMaxCoin)
      val combosWithoutMaxCoin = countChange(money, coinsMinusMaxCoin)
      combosWithMaxCoin + combosWithoutMaxCoin
    }

  // Wanted to see if I could get this algorithm also producing the list of coins in the combinations.
  def changeCombinations(money: Int, coins: List[Int]): List[List[Int]] =
    if (money == 0) {
      List(List())
    } else if (coins.isEmpty) { // || !coins.exists { coin => money % coin == 0 }) {
      List()
    } else {
      val maxCoin = coins.max
      val coinsMinusMaxCoin = coins.filterNot { x => x == maxCoin }
      val rem = money - maxCoin
      val combosWithMaxCoin = changeCombinations(rem, if (rem >= maxCoin) coins else coinsMinusMaxCoin)
          .map { combo => maxCoin +: combo }
      val combosWithoutMaxCoin = changeCombinations(money, coinsMinusMaxCoin)

      combosWithMaxCoin ++ combosWithoutMaxCoin
    }
}

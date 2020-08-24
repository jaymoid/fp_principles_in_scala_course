import scala.annotation.tailrec

// Worksheets dont seem to work with recent version of scala
object  SqRoot {

  def abs(x:Double)  = if (x < 0) -x else x

  def sqrt(n: Double): Double = {
    @tailrec
    def sqrtIter(guess: Double, n: Double): Double = {
      if (isGoodEnough(guess, n)) guess
      else sqrtIter(improve(guess, n), n)
    }

    def isGoodEnough(guess: Double, n: Double): Boolean =
      abs(guess * guess - n) < 0.001

    def improve(guess: Double, n: Double): Double =
      (guess + n / guess) / 2
    // eg (4.98 + 25 / 4.98 ) / 2
    // evaluates to
    //    (4.98 + (25 / 4.98 )) / 2
    //    (4.98 + 5.02008) / 2
    //    (4.98 + 5.02008) / 2
    //    (10.000080321285141) / 2
    //    5.00004016064257

    sqrtIter(1.0, n)
  }

  sqrt(25.0)
  //  println(sqrt(25.0))
}

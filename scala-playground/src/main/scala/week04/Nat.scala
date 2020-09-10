package week04


abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat  = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  override def isZero: Boolean = true

  override def predecessor: Nat = throw new NoSuchElementException("Zero has no natural predecessor, you want an integer if you need negative numbers?")

  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat = if (that == Zero) this else throw new NoSuchElementException("Cannot subtract any natural from Zero, which isn't zero.")
}

class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false

  override def predecessor: Nat = n

  override def +(that: Nat): Nat = new Succ(n + that)

  override def -(that: Nat): Nat = if (that == Zero) this else n - that.predecessor
}

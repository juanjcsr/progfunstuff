package week4.idelized.scala
// PEANO NUMBERS!!!!!!!!!!!
/**
  * Created by jezz on 6/06/16.
  */
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  override def isZero: Boolean = true

  override def successor: Nat = new Succ(this)

  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat = if (that.isZero) this else throw new Error("negative")

  override def predecessor: Nat = throw new Error("0.predecessor")
}

class Succ(x: Nat) extends Nat {

  override def isZero: Boolean = false

  override def successor: Nat = new Succ(this)

  override def +(that: Nat): Nat = new Succ(x + that)

  override def -(that: Nat): Nat = if (that.isZero) this else x - that.predecessor

  override def predecessor: Nat = x
}

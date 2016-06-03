package week3

/**
  * Created by jezz on 3/06/16.
  */
class Rational(x: Int, y: Int) {
  require( y != 0, "denominator must be nonzero")
  private val g = gcd(x, y)
  //def numer = x / g
  //def denom = y / g
  //changing def to var
  def numer = x / g
  def denom = y / g

  //override the constructor to simplify the rational
  def this(x: Int) = this(x, 1)

  def add(that: Rational): Rational =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  def neg: Rational =
    new Rational(- numer, - denom)
  //this overrides the - symbol
  def unary_- : Rational =
    new Rational(- numer, - denom)

  def sub(that: Rational): Rational =
    add(that.neg)

  def - (that: Rational): Rational =
    add(that.neg)

  def less(that: Rational) =
    numer * that.denom < that.numer * denom

  //rewrite < symbol
  def < (that: Rational) =
    numer * that.denom < that.numer * denom

  def max(that: Rational) =
    if (this < that ) that else this

  override def toString = numer + "/" + denom

  private def gcd(a: Int, b: Int): Int =
    if ( b == 0) a
    else gcd(b, a % b)
}

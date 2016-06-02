import math.abs


  val tolerance = 0.0001

  def isCloseEnough(x: Double, y: Double) = {
    abs((x-y) / x) / x < tolerance
  }

  //Fixed point of a function
  // f(x) = x
  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      println("guess = " + guess )
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }

  //f(x) = 1 + x/2
  fixedPoint(x=>1 + x/2)(2)


  //Square roots
  //Specification sqrt(x) = the number 'y' such that y * y = x
  // or, dividing with y:
  //   sqrt(x) = the number 'y' such that y = x / y
  // Therefore:
  //   sqrt(x) is a fixed point of the function (y => x / y)

  //bad sqrt with fixed point, because it moves between 1 and 2
  // we can modify by gettng the average
  def sqrt(x: Double) = fixedPoint(y => (y + x  / y)/ 2)(1)
  sqrt(2)

  //Now we need to create a function to average damping functions
  def averageDamp(f: Double => Double)(x: Double ) = (x + f(x)) / 2
  def dampSqrt(x: Double) =
    fixedPoint(averageDamp(y => x / y))(1)

  dampSqrt(2)


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


  val x = new Rational(1,3)
  val y = new Rational(5,6)
  val z = new Rational(3,2)
  x.numer
  x.denom

  x.add(y)
  x.sub(y).sub(z)
  y.add(y)
  x.less(y)


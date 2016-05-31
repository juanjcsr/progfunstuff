import math.abs

object exercise {
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
}
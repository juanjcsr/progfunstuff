import scala.annotation.tailrec

object session {
  1+3
  def abs(x: Double) = if (x < 0 ) -x else x

  def sqrtIter(guess: Double, x: Double): Double = {
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)

  }

  //The problem with this function is that it takes the absolute
  //value and the 0.001 value might be bigger than a small number, or
  //the distance between two big numbers is very large
  def isGoodEnough(guess: Double, x: Double) = {
    //abs(guess * guess - x) < 0.001
    abs(guess * guess  -x) / x < 0.001
  }


  def improve(guess: Double, x: Double) = {
    (guess + x / guess) / 2
  }

  def sqrt(x: Double) = sqrtIter(1.0, x)

  sqrt(2)
  sqrt(4)
  sqrt(1e-6)
  sqrt(1e60)

  //It is a good functional prog style to split a task into many
  //small functions

  //namespacing with a block and eliminating duplicates:
  def sqrtBlock(x: Double) = {
    def sqrtIter(guess: Double): Double = {
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    }

    //The problem with this function is that it takes the absolute
    //value and the 0.001 value might be bigger than a small number, or
    //the distance between two big numbers is very large
    def isGoodEnough(guess: Double) = {
      //abs(guess * guess - x) < 0.001
      abs(guess * guess  -x) / x < 0.001
    }


    def improve(guess: Double) = {
      (guess + x / guess) / 2
    }
    sqrtIter(1.0)
  }

  sqrtBlock(2)
  sqrtBlock(4)
  sqrtBlock(1e-6)
  sqrtBlock(1e60)


  //RECURSION
  @tailrec
  def gcd(a: Int, b: Int) : Int = {
    if (b == 0) a else gcd(b, a % b)
  }
  gcd(14,21)

  //Not Tail-recursive
  def factorial(n : Int): Int = {
    if (n == 0) 1 else n * factorial(n - 1)
  }

  //Tail recursive factorial
  def facttail(n: Int): Int = {
    @tailrec
    def facttailRec(acc: Int, n: Int): Int = {
      if (n == 0) acc
      else facttailRec(acc * n, n - 1)
    }
    facttailRec(1, n)
  }
  factorial(4)
  facttail(10)
}
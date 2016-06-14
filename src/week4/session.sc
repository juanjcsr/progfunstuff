//function are objects in scala
//they are objects with apply methods

//Type parameters (any parameters)
trait List[T] {
  //we need to define if the list is empty or not
  def isEmpty : Boolean

  //if we have a non-empty list, we need to find the
  //head
  def head: T

  //And we need the tail
  def tail: List[T]
}
class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")
}

object List {
  //List(1,2) = List.apply(1,2)
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))
  def apply[T](x1: T) = new Cons(x1, new Nil)
  def apply[T]() = new Nil
}

val a = List(1,2)
val b = a.head

//Decomposition

trait Expr {
  def isNumber: Boolean
  def isSum: Boolean
  def numValue: Int
  def leftOp: Expr
  def rightOp: Expr

  //pattern matching for operations
  def eval: Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
  }
}
//To match clases, they must be implemented as cases
case class Number(n: Int) extends Expr {
  override def isNumber: Boolean = true

  override def rightOp: Expr = throw new Error("Number.rightOp")

  override def numValue: Int = n

  override def isSum: Boolean = false

  override def leftOp: Expr = throw new Error("Number.leftOp")
}

// new Sum(e1, e2) ========= e1 + e2
case class Sum(n1: Expr, n2: Expr) extends Expr{
  override def isNumber: Boolean = false

  override def rightOp: Expr = n1

  override def numValue: Int = throw new Error("Sum.numValue")

  override def isSum: Boolean = true

  override def leftOp: Expr = n2
}

def eval(e: Expr): Int = {
  if (e.isNumber) e.numValue
  else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
  else throw new Error("Unknow exception" + e)
}

eval(new Sum(new Number(1), new Number(2)))
//If we add more operations, we must create 35 methods for
//each class and trait
//isProduct
//isDiff


//PATTERN MATCHING
//we use case clases

def show(e: Expr): String = e match {
  case Number(n)    => n.toString()
  case Sum(e1, e2)  => show(e1) + " + " + show(e2)
}

show(new Sum(new Number(2), new Number(2)) )
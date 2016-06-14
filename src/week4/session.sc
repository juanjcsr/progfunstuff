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
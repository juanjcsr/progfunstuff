import java.util.NoSuchElementException

//import week3.Rational
//import everything:
//import week3.Rational


  //new Rational(1,2)

  def error(msg: String) = throw new Error(msg)

  val x = null

if (true) 1 else false


//Traits are like abstract classes.
//but they let us implement "multiple hierachy"
//Traits can only contain fields and concrete methods
//Traits cannot have value parameters, only classes can
//They are "like" interfaces


class Cons0(val head: Int, _tail: Int) {

}
//This is equivalent to:
class Cons1(_head: Int, _tail: Int) {
  val head = _head
  val tail = _tail
}

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


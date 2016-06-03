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

//functions can also have Type parameters
def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])
singleton[Int](1)
singleton(true)
singleton("Hola")

//Type parameters do not affect evaluation
// they are removed before evaluating the program
// This is called "Type erasure"

//Type parameters is a form of Polymorphism

//Ex: write a function nth that takes an integer and a
// list and selects the nth element of the list
def takeNth[T](n: Int, list: List[T]): T = {
  if(list.isEmpty) throw new IndexOutOfBoundsException
  if (n == 0) list.head
  else takeNth(n - 1, list.tail )
}

val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))

takeNth(2, list)
takeNth(-1, list)
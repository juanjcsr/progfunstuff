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


class Cons(val head: Int, _tail: Int) {

}
//This is equivalent to:
class Cons1(_head: Int, _tail: Int) {
  val head = _head
  val tail = _tail
}
//Abstract class can contain members w/o implementation
// "IntSet is a superclass"
abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean

  //TASK!
  //Create a method called "union"
  def union(other: IntSet): IntSet
}

//Implementing Sets as binary trees:
//A tree for emptu set
//A tree consisting of an int and two sub-trees
//This implementation is purely functional so each call to
// the include methods, will generate a new tree with the
// new reference. These data structures are called
// "Persistent Data Structures"

//Since there will only be one "Empty" set, we can define
//a singleton. A singleton is defined as an object
//class Empty extends IntSet {
object Empty extends IntSet{
  def contains(x: Int): Boolean = false

  //def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

  def union(other: IntSet): IntSet = {
    other
  }

  //Method to print an empty tree
  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet{
  def contains(x: Int): Boolean = {
    //Check if X exists in the left side of the tree, else
    // check on the right
    if (x < elem) left contains x
    else if ( x > elem ) right contains(x)
    else true
  }

  //Insert elements in order
  def incl(x: Int): IntSet =
    if ( x< elem ) new NonEmpty(elem, left.incl(x), right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  def union(other: IntSet): IntSet = {
    ((left union right) union other) incl elem
  }

  //Method to print a tree
  override def toString =
    "{" + left +  elem + right + "}"
}

val t1 = new NonEmpty(3, Empty, Empty)
val t2 = t1 incl 4
val t3 = t2.incl(2)

//"object" definitions let us create "standalone applications"
object Hello {
  def main(args: Array[String]) = println("oloo")
}


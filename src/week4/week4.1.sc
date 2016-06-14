//LISTS
val fruit = List("manzana", "peras", "sandiaa")
val nums = List(1,2,3,4)
val diag3 = List(List(1,0,0), List(0,1,0), List(0,0,1))
//list are inmutable
//list are recursive

//The lists are constructed from
// The empty list Nil
// the construction operation ::

val fruit1 = "manzana" :: ("peras" :: ("sandiaa" :: Nil))

fruit.head
fruit.tail

def insertionSort(xs: List[Int]): List[Int] = xs match{
  case List() => List()
  case y :: ys => insert(y, insertionSort(ys))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
}


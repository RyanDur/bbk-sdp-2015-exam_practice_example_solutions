// A partial function is a function that is valid for only a subset
// of values of those types you might pass in to it.
// http://stackoverflow.com/questions/8650549/using-partial-functions-in-scala-how-does-it-work

val list: List[Any] = List(1, "poo", Nil, 4, 0.2)

list.collect { case x: Int => x * 2 }

list.collect { case x: String => x + " now" }

list.collect { case Nil => " Blow up" }

val root: PartialFunction[Any, Double] = {
  case d: Double if d >= 0 => math.sqrt(d)
}

val ls = List[Double](0.5, -0.2, 4)

val rtls = ls.collect(root)
list.collect(root)

// The main distinction between PartialFunction and Function
// is that the user of a PartialFunction may choose to do something
// different with input that is declared to be outside its domain.

// For example:

val sample = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
val isEven: PartialFunction[Int, String] = {
  case x if x % 2 == 0 => x + " is even"
}

// the method collect can use isDefinedAt to select which members to collect
val evenNumbers = sample collect isEven

val isOdd: PartialFunction[Int, String] = {
  case x if x % 2 == 1 => x + " is odd"
}

// the method orElse allows chaining another partial function to handle
// input outside the declared domain

sample.map(isEven.orElse(isOdd))
// the same as, but less elegant
sample.map {
  case x if x % 2 == 0 => x + " is even"
  case x if x % 2 == 1 => x + " is odd"
}

//http://www.scala-lang.org/api/current/index.html#scala.PartialFunction

// A partial function of type PartialFunction[A, B] is a unary
// function where the domain does not necessarily include all
// values of type A.(e.g. A partial function is a function that is
// valid for only a subset of values of those types you might pass in to it.)
//
// If you are to use PartialFunction in a method you are implementing -
// The function 'isDefinedAt' allows to test
// dynamically if a value is in the domain of the function.

// def func(pf: PartialFunction[A, B]): something {
//   maybe some stuff like cases or something
//   if (pf.isDefinedAt(h) ) { //if it's the correct type
//      do this
//    } else {
//      do that
//    }
//   }

//Collect
def collect(list: List[Any], pf: PartialFunction[Any, String]): List[String] =
  list match {
    case Nil => Nil
    case hd :: tl =>
      if (pf.isDefinedAt(hd)) pf(hd) :: collect(tl, pf)
      else collect(tl, pf)
  }

val isEvenAlso: PartialFunction[Any, String] = {
  case x: Int if x % 2 == 0 => x + " is even"
}
collect(list, isEvenAlso)




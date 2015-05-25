package main.scala

final case class Leaf(r: BST, value: Int, l: BST) extends BST {
  val size: Int = 1 + r.size + l.size
}

case object EmptyLeaf extends BST {
  val size: Int = 0
}

sealed trait BST {
  def size: Int

  def add(inValue: Int): BST = this match {
    case EmptyLeaf => Leaf(EmptyLeaf, inValue, EmptyLeaf)
    case Leaf(l, v, r) if v > inValue => Leaf(l.add(inValue), v, r)
    case Leaf(l, v, r) => Leaf(l, v, r.add(inValue))
  }

  def contains(inValue: Int): Boolean = this match {
    case EmptyLeaf => false
    case Leaf(_, v, _) if v == inValue => true
    case Leaf(l, v, _) if v > inValue => l.contains(inValue)
    case Leaf(_, v, r) => r.contains(inValue)
  }

  // LHR
  def inOrder(f: Int => Unit): Unit = this match {
    case EmptyLeaf =>
    case Leaf(l, h, r) => l.inOrder(f); f(h); r.inOrder(f)
  }

  // HLR
  def preOrder(f: Int => Unit): Unit = this match {
    case EmptyLeaf =>
    case Leaf(l, h, r) => f(h); l.preOrder(f); r.preOrder(f)
  }

  // LRH
  def postOrder(f: Int => Unit): Unit = this match {
    case EmptyLeaf =>
    case Leaf(l, h, r) => l.postOrder(f); r.postOrder(f); f(h)
  }

  def depth: Int = this match {
    case EmptyLeaf => 0
    case Leaf(l, _, r) => 1 + (l.depth max r.depth)
  }

  def map(f: Int => Int): BST = this match {
    case EmptyLeaf => EmptyLeaf
    case Leaf(l, v, r) => Leaf(l.map(f), f(v), r.map(f))
  }
}

object BST {
  def apply(): BST = EmptyLeaf
}

object Runner extends App {
  var tree = BST()
  tree = tree.add(4)
  tree = tree.add(9)
  tree = tree.add(3)
  tree = tree.add(1)
  tree = tree.add(10)

  println(tree)
  println()
  println("in")
  tree.inOrder(x => print(x + " "))
  println()
  println()
  println("pre")
  tree.preOrder(x => print(x + " "))
  println()
  println()
  println("post")
  tree.postOrder(x => print(x + " "))
  println()
  println()
  println(tree.contains(10))
  println(tree.contains(412))
}


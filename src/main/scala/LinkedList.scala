package main.scala

import scala.annotation.tailrec

final case class Node[+A](head: A, tail: LinkedList[A]) extends LinkedList[A] {

  val size = 1 + tail.size

  val isEmpty: Boolean = false
}

case object Empty extends LinkedList[Nothing] {
  val size = 0

  val isEmpty: Boolean = true

  override def head: Nothing =
    throw new NoSuchElementException("head of empty list")

  override def tail: LinkedList[Nothing] =
    throw new UnsupportedOperationException("tail of empty list")
}

sealed trait LinkedList[+A] {

  def size: Int

  def isEmpty: Boolean

  def tail: LinkedList[A]

  def head: A

  def toList: List[A] = {
    //    case Empty => List()
    //    case Node(h, tl) => h :: tl.toList
    foldRight(List[A]())((b, acc) => b :: acc)
  }

  def setHead[B >: A](v: B): LinkedList[B] = this match {
    case Empty => LinkedList[B](v)
    case Node(h, tl) => v :: tl
  }

  def ::[B >: A](element: B): LinkedList[B] = Node(element, this)

  @tailrec final def dropWhile(f: A => Boolean): LinkedList[A] = this match {
    case Empty => Empty
    case Node(h, tl) => if (f(h)) tl.dropWhile(f) else this
  }

  def ++[B >: A](that: LinkedList[B]): LinkedList[B] = {
    @tailrec def helper(acc: LinkedList[B], other: LinkedList[B]): LinkedList[B] = other match {
      case Empty => acc
      case head Node tail => helper(head :: acc, tail)
    }
    helper(that, this.reverse())
    //    (this, that) match {
    //      case (Empty, Empty) => Empty
    //      case (Empty, Node(_, _)) | (Node(_, _), Empty) => if (this.isEmpty) that else this
    //      case (Node(h, tl), Node(h2, tl2)) => h :: tl ++ that
  }

  @tailrec final def foldLeft[B](acc: B)(f: (B, A) => B): B = this match {
    case Empty => acc
    case Node(h, tl) => tl.foldLeft(f(acc, h))(f)
  }

  def fold[B >: A](acc: B)(f: (B, B) => B): B =
    foldLeft(acc)(f)

  def reverse(): LinkedList[A] =
    foldLeft(LinkedList[A]())((acc, item) => item :: acc)

  def foldRight[B](acc: B)(f: (A, B) => B): B =
    reverse().foldLeft(acc)((acc, item) => f(item, acc))

  def reduce[B >: A](f: (B, B) => B): B = this match {
    case Empty => throw new UnsupportedOperationException("empty.reduceLeft")
    case Node(h, Empty) => h
    case Node(h, tl) => tl.foldRight(h)((item, acc) => f(acc, item).asInstanceOf[A])
    //    def helper[B >: A](list: LinkedList[B], f: (B, B) => B): B = list match {
    //      case Empty => throw new UnsupportedOperationException("empty.reduceLeft")
    //      case Node(h, Empty) => h
    //      case Node(h, tl) => f(helper(tl, f), h)
    //    }
    //    helper(this.reverse(), f)
  }

  def filter(p: A => Boolean): LinkedList[A] =
    foldRight(LinkedList[A]()) { (item, acc) =>
      if (p(item)) item :: acc
      else acc
    }

  def find(p: (A) => Boolean): Option[A] = this match {
    case Empty => None
    case Node(h, tl) => if (p(h)) Some(h) else tl.find(p)
  }

  def map[B](f: A => B): LinkedList[B] = this match {
    case Empty => Empty
    case Node(h, tl) => f(h) :: tl.map(f)
  }

  def flatten[B]: LinkedList[B] = this match {
    case Node(h, tl) => h.asInstanceOf[LinkedList[B]] ++ tl.flatten
    case _ => Empty
  }

  def flatMap[B](f: A => LinkedList[B]): LinkedList[B] = {
    map(x => f(x)).flatten
    //    foldLeft(LinkedList[B]())((list, v1) => list ++ f(v1))
  }

  def negate[B >: A](predicate: B => Boolean): B => Boolean =
    (x: B) => !predicate(x)

  def partition(p: (A) => Boolean): (LinkedList[A], LinkedList[A]) =
    (filter(p), filter(negate(p)))

  // Manually test
  def foreach(f: A => Unit): Unit = {
    @tailrec def loop(items: LinkedList[A]): Unit = {
      items match {
        case Node(h, tl) =>
          f(h)
          loop(tl)
        case Empty =>
      }
    }
    loop(this)
  }

  def zip[B](that: LinkedList[B]): LinkedList[(A, B)] = (this, that) match {
    case (Empty, _) | (_, Empty) => Empty
    case (Node(h1, tl1), Node(h2, tl2)) => (h1, h2) :: (tl1 zip tl2)
  }

  def collect[B](pf: PartialFunction[A, B]): LinkedList[B] = this match {
    case Empty => Empty
    case Node(h, tl) =>
      if (pf.isDefinedAt(h)) pf(h) :: tl.collect(pf)
      else tl.collect(pf)
  }

  def groupBy[K](f: (A) => K): Map[K, LinkedList[A]] = this match {
    case Empty => Map()
    case Node(h, tl) =>
      val key = f(h)
      val map = tl.groupBy(f)
      map ++ Map(key -> (h :: map.getOrElse(key, Empty)))
  }

  def splitAt(n: Int): (LinkedList[A], LinkedList[A]) = this match {
    case Empty => (Empty, Empty)
    case Node(h, tl) =>
      if (n > 0) {
        val (left, right) = tl.splitAt(n - 1)
        (h :: left, right)
      } else (Empty, this)
  }

  def mergeSort(): LinkedList[Int] = {
    def merge(left: LinkedList[Int], right: LinkedList[Int]): LinkedList[Int] = (left, right) match {
      case (Node(h1, tl1), Node(h2, tl2)) =>
        if (h1 <= h2) h1 :: merge(tl1, right)
        else h2 :: merge(left, tl2)
      case _ => if (left.isEmpty) right else left
    }
    def sort(input: LinkedList[Int], length: Int): LinkedList[Int] = input match {
      case Empty | Node(_, Empty) => input
      case _ =>
        val middle = length / 2
        val (left, right) = input splitAt middle
        merge(sort(left, middle), sort(right, middle + length % 2))
    }

    sort(this.asInstanceOf[LinkedList[Int]], this.size)
  }
}

object LinkedList {
  def apply[A](items: A*): LinkedList[A] =
    items.foldRight(Empty: LinkedList[A])(_ :: _)
}
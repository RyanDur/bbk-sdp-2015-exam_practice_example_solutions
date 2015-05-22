package main.scala

import java.util.NoSuchElementException

import org.scalatest.{ShouldMatchers, path}

class LinkedListSpec extends path.FunSpec with ShouldMatchers {
  describe("A Linked List") {
    var list: LinkedList[Int] = null
    describe("on creation") {
      describe("When Empty") {
        list = LinkedList()

        it("should have size 0")(list.size should equal(0))
        it("should know its empty")(list.isEmpty shouldBe true)
        it("should not have a head")(intercept[NoSuchElementException]
          (list.head).getMessage should equal("head of empty list"))
        it("should not have a tail")(intercept[UnsupportedOperationException]
          (list.tail).getMessage should equal("tail of empty list"))
        it("should be equivalent to List") {
          val h1 = intercept[NoSuchElementException](list.head)
          val h2 = intercept[NoSuchElementException](List().head)
          val tl1 = intercept[UnsupportedOperationException](list.tail)
          val tl2 = intercept[UnsupportedOperationException](List().tail)

          h1.getCause should equal(h2.getCause)
          h1.getMessage should equal(h2.getMessage)
          tl1.getCause should equal(tl2.getCause)
          tl1.getMessage should equal(tl2.getMessage)
          list.isEmpty should equal(List().isEmpty)
          list.size should equal(List().size)
        }
      }
      describe("with one element") {
        list = LinkedList(5)

        it("should have size 1")(list.size should equal(1))
        it("should know its not empty")(list.isEmpty shouldBe false)
        it("should have a head")(list.head should equal(5))
        it("should not have an empty tail")(list.tail shouldBe Empty)
        it("should be equivalent to List") {
          list.toList.head shouldBe List(5).head
          list.toList.tail shouldBe List(5).tail
          list.isEmpty should equal(List(5).isEmpty)
          list.size should equal(List(5).size)
        }
      }

      describe("with multiple elements") {
        list = LinkedList(3, 9, 4, 5)

        it("should have a size of 4")(list.size should equal(4))
        it("should know its not empty")(list.isEmpty shouldBe false)
        it("should have a head")(list.head should equal(3))
        it("should not have an empty tail")(list.tail should equal(Node(9, Node(4, Node(5, Empty)))))
        it("should be equivalent to List") {
          list.toList.head shouldBe List(3, 9, 4, 5).head
          list.toList.tail shouldBe List(3, 9, 4, 5).tail
          list.isEmpty should equal(List(3, 9, 4, 5).isEmpty)
          list.size should equal(List(3, 9, 4, 5).size)
        }
      }
    }

    describe("toList") {
      describe("When Empty") {
        list = LinkedList()
        it("should return an empty list")(list.toList should equal(List()))
      }
      describe("with one element") {
        list = LinkedList(3)
        it("should return a list with the same element")(list.toList should equal(List(3)))
      }
      describe("with multiple elements") {
        list = LinkedList(1, 2, 3)
        it("should return a list with the same elements in the same order")(list.toList should equal(List(1, 2, 3)))
      }
    }

    describe("setHead") {
      describe("When Empty") {
        list = LinkedList()
        list = list.setHead(4)
        it("should be the only element")(list.size should equal(1))
        it("should be in the head")(list.head should equal(4))
        it("should have an empty tail")(list.tail should equal(Empty))
      }
      describe("with one element") {
        list = LinkedList(5)
        list = list.setHead(9)
        it("should only have one element")(list.size should equal(1))
        it("should replace the existing head")(list.head should equal(9))
        it("should have an empty tail")(list.tail should equal(Empty))
      }
      describe("with multiple elements") {
        list = LinkedList(4, 8, 2)
        val list2 = list.setHead(10)
        it("should have the same amount of elements")(list2.size should equal(list.size))
        it("should replace the existing head") {
          list2.head should not equal list.head
          list.head should equal(4)
          list2.head should equal(10)
        }
        it("should have the same tail")(list2.tail should equal(list.tail))
      }
    }

    describe(":: - Adds an element at the beginning of this list.") {
      describe("When Empty") {
        list = LinkedList()
        it("should prepend one element")(list.::(3) should equal(Node(3, Empty)))
        it("should be equivalent to List")(list.::(3).toList should equal(List().::(3)))
      }
      describe("with one element") {
        list = LinkedList(4)
        it("should prepend one element")(list.::(3) should equal(Node(3, Node(4, Empty))))
        it("should be equivalent to List")(list.::(3).toList should equal(List(4).::(3)))
      }
      describe("with multiple elements") {
        list = LinkedList(4, 5, 6, 7)
        it("should prepend one element")(list.::(3) should equal(Node(3, Node(4, Node(5, Node(6, Node(7, Empty)))))))
        it("should be equivalent to List")((3 :: 1 :: 2 :: Empty).toList should equal(List(3, 1, 2)))
      }
      it("should be right associative") {
        (3 :: 1 :: 2 :: Empty) should equal(Node(3, Node(1, Node(2, Empty))))
      }
    }

    describe("dropWhile - Drops longest prefix of elements that satisfy a predicate.") {
      describe("When Empty") {
        list = LinkedList()
        it("should not do anything")(list.dropWhile(a => a < 3) should equal(list))
        it("should be equivalent to List") {
          list.dropWhile(a => a < 3).toList should equal(List[Int]().dropWhile(a => a < 3))
        }
      }
      describe("with one element") {
        list = LinkedList(3)
        it("should not drop if it does not satisfy condition")(list.dropWhile(a => a < 3) should equal(list))
        it("should drop if it does satisfy condition")(list.dropWhile(a => a < 4) should equal(Empty))
        it("should be equivalent to List") {
          list.dropWhile(a => a < 3).toList should equal(List(3).dropWhile(a => a < 3))
          list.dropWhile(a => a < 4).toList should equal(List(3).dropWhile(a => a < 4))
        }
      }
      describe("with multiple elements") {
        list = LinkedList(0, 1, 2, 3, 4, 2, 1)
        it("should drop longest prefix of elements that satisfy the predicate") {
          list.dropWhile(a => a < 4) should equal(Node(4, Node(2, Node(1, Empty))))
          list.dropWhile(a => a < 5) should equal(Empty)
        }
        it("should be able to handle large amount of information") {
          LinkedList(0 to 1000000: _*).dropWhile(a => a < 1000000) should equal(Node(1000000, Empty))
        }
        it("should be equivalent to List") {
          LinkedList(0, 1, 2, 3, 4, 2, 1).dropWhile(a => a < 4).toList should
            equal(List(0, 1, 2, 3, 4, 2, 1).dropWhile(a => a < 4))
          LinkedList(0, 1, 2, 3, 4, 2, 1).dropWhile(a => a < 5).toList should
            equal(List(0, 1, 2, 3, 4, 2, 1).dropWhile(a => a < 5))
        }
      }
    }

    describe("++ - Returns a new list containing the elements from the left hand operand followed by the " +
      "elements from the right hand operand.") {
      val listToBAdded = LinkedList(1, 2, 3)
      describe("When Empty") {
        list = LinkedList()
        it("should take the form of the added list")((list ++ listToBAdded) should equal(listToBAdded))
        it("should be empty if both lists are empty")((list ++ LinkedList()) should equal(Empty))
        it("should be equivalent to List") {
          (list ++ listToBAdded).toList should equal(List() ++ List(1, 2, 3))
          (list ++ LinkedList()).toList should equal(List() ++ List())
        }
      }
      describe("with one element") {
        list = LinkedList(3)
        it("should take the form of the original list if added list is empty") {
          (list ++ LinkedList()) should equal(list)
        }
        it("should add the two lists together")((list ++ listToBAdded) should equal(LinkedList(3, 1, 2, 3)))
        it("should be equivalent to List") {
          (list ++ listToBAdded).toList should equal(List(3) ++ List(1, 2, 3))
          (list ++ LinkedList()).toList should equal(List(3) ++ List())
        }
      }
      describe("with multiple elements") {
        list = LinkedList(3, 9, 5, 7)
        it("should add the two lists together")((list ++ listToBAdded) should equal(LinkedList(3, 9, 5, 7, 1, 2, 3)))
        it("should be equivalent to List") {
          (list ++ listToBAdded).toList should equal(List(3, 9, 5, 7) ++ List(1, 2, 3))
        }
      }
    }

    describe("foldLeft - Applies a binary operator to a start value and all elements of this sequence, going left to right.") {
      describe("When Empty") {
        it("should return the accumulator unchanged")(LinkedList[Int]().foldLeft(0)((a, b) => a - b) should equal(0))
        it("should be equivalent to List") {
          LinkedList[Int]().foldLeft(0)((a, b) => a - b) should equal(List[Int]().foldLeft(0)((a, b) => a - b))
        }
      }
      describe("with one element") {
        it("should return the element unchanged")(LinkedList[Int](3).foldLeft(0)((a, b) => a - b) should equal(-3))
        it("should be equivalent to List") {
          LinkedList[Int](3).foldLeft(0)((a, b) => a - b) should equal(List[Int](3).foldLeft(0)((a, b) => a - b))
        }
      }
      describe("with multiple elements") {
        it("should perform the the function on each element accumulating it into one value") {
          LinkedList[Int](1, 2, 3).foldLeft(0)((a, b) => a - b) should equal(-6)
          LinkedList[String]("Hi", "There").foldLeft(" ")((a, b) => a + b) should equal(" HiThere")
        }
        it("should be equivalent to List") {
          LinkedList[Int](1, 2, 3).foldLeft(0)((a, b) => a - b) should
            equal(List(1, 2, 3).foldLeft(0)((a, b) => a - b))
        }
      }
    }

    describe("fold - Folds the elements of this traversable or iterator using the specified associative binary operator.") {
      describe("When Empty") {
        it("should return the accumulator unchanged")(LinkedList[Int]().fold(0)((a, b) => a - b) should equal(0))
        it("should be equivalent to List") {
          LinkedList[Int]().fold(0)((a, b) => a - b) should equal(List[Int]().fold(0)((a, b) => a - b))
        }
      }
      describe("with one element") {
        it("should return the element unchanged")(LinkedList[Int](3).fold(0)((a, b) => a - b) should equal(-3))
        it("should be equivalent to List") {
          LinkedList[Int](3).fold(0)((a, b) => a - b) should equal(List[Int](3).fold(0)((a, b) => a - b))
        }
      }
      describe("with multiple elements") {
        it("should perform the the function on each element accumulating it into one value") {
          LinkedList[Int](1, 2, 3).fold(0)((a, b) => a - b) should equal(-6)
          LinkedList[String]("Hi", "There").fold(" ")((a, b) => a + b) should equal(" HiThere")
        }
        it("should be equivalent to List") {
          LinkedList[Int](1, 2, 3).fold(0)((a, b) => a - b) should
            equal(List(1, 2, 3).fold(0)((a, b) => a - b))
        }
      }
    }

    describe("reverse - Returns new list with elements in reversed order.") {
      describe("When Empty") {
        list = LinkedList()
        it("should return an empty list")(list.reverse() shouldBe Empty)
        it("should be equivalent to List")(list.reverse().toList shouldBe List().reverse)
      }
      describe("with one element") {
        list = LinkedList(3)
        it("should not affect the list")(list.reverse() should equal(list))
        it("should be equivalent to List")(list.reverse().toList shouldBe List(3).reverse)
      }
      describe("with multiple elements") {
        list = LinkedList(1, 2, 3)
        it("should reverse the order of the list")(list.reverse() should equal(Node(3, Node(2, Node(1, Empty)))))
        it("should be equivalent to List") {
          LinkedList[Int](1, 2, 3).reverse().toList should equal(List(1, 2, 3).reverse)
        }
      }
    }

    describe("foldRight - Applies a binary operator to all elements of this list and a start value, going right to left.") {
      describe("When Empty") {
        it("should return the accumulator unchanged")(LinkedList[Int]().foldRight(0)((a, b) => a - b) should equal(0))
        it("should be equivalent to List") {
          LinkedList[Int]().foldRight(0)((a, b) => a - b) should equal(List[Int]().foldRight(0)((a, b) => a - b))
        }
      }
      describe("with one element") {
        it("should return the element unchanged")(LinkedList[Int](3).foldRight(0)((a, b) => a - b) should equal(3))
        it("should be equivalent to List") {
          LinkedList[Int](3).foldRight(0)((a, b) => a - b) should equal(List[Int](3).foldRight(0)((a, b) => a - b))
        }
      }
      describe("with multiple elements") {
        it("should perform the the function on each element accumulating it into one value") {
          LinkedList[Int](1, 2, 3).foldRight(0)((a, b) => a - b) should equal(2)
          LinkedList[String]("Hi", "There").foldRight(" ")((a, b) => a + b) should equal("HiThere ")
        }
        it("should be equivalent to List") {
          LinkedList[Int](1, 2, 3).foldRight(0)((a, b) => a - b) should
            equal(List(1, 2, 3).foldRight(0)((a, b) => a - b))
        }
      }
    }

    describe("reduce - Reduces the elements of this traversable or iterator using the specified " +
      "associative binary operator.") {
      describe("When Empty") {
        it("should return the accumulator unchanged") {
          intercept[UnsupportedOperationException](LinkedList[Int]().reduce((a, b) => a - b))
            .getMessage shouldBe "empty.reduceLeft"
        }
        it("should be equivalent to List") {
          intercept[UnsupportedOperationException](LinkedList[Int]().reduce((a, b) => a - b))
            .getCause shouldBe intercept[UnsupportedOperationException](List[Int]().reduce((a, b) => a - b))
            .getCause
          intercept[UnsupportedOperationException](LinkedList[Int]().reduce((a, b) => a - b))
            .getMessage shouldBe intercept[UnsupportedOperationException](List[Int]().reduce((a, b) => a - b))
            .getMessage
        }
      }
      describe("with one element") {
        it("should return the element unchanged")(LinkedList[Int](3).reduce((a, b) => a - b) should equal(3))
        it("should be equivalent to List") {
          LinkedList[Int](3).reduce((a, b) => a - b) should equal(List[Int](3).reduce((a, b) => a - b))
        }
      }
      describe("with multiple elements") {
        it("should perform the the function on each element accumulating it into one value") {
          LinkedList[Int](1, 2, 3, 4).reduce((a, b) => a - b) should equal(-8)
          LinkedList[String]("Hi", "There").reduce((a, b) => a + b) should equal("HiThere")
        }
        it("should be equivalent to List") {
          LinkedList[Int](1, 2, 3, 4).reduce((a, b) => a - b) should
            equal(List(1, 2, 3, 4).reduce((a, b) => a - b))
        }
      }
    }

    describe("filter - Selects all elements of this traversable collection which satisfy a predicate.") {
      describe("When Empty") {
        list = LinkedList()
        it("should return empty if empty")(list.filter(a => a % 2 == 0) should equal(Empty))
        it("should be equivalent to List") {
          list.filter(a => a % 2 == 0).toList should equal(List[Int]().filter(a => a % 2 == 0))
        }
      }
      describe("with one element") {
        list = LinkedList(2)
        it("should filter based on the predicate")(list.filter(a => a % 2 == 0) should equal(Node(2, Empty)))
        it("should be empty if it doesn't satisfy the predicate")(list.filter(a => a % 3 == 0) should equal(Empty))
        it("should be equivalent to List") {
          list.filter(a => a % 2 == 0).toList should equal(List[Int](2).filter(a => a % 2 == 0))
          list.filter(a => a % 3 == 0).toList should equal(List[Int](2).filter(a => a % 3 == 0))
        }
      }
      describe("with multiple elements") {
        list = LinkedList(1, 2, 3, 4)
        it("should filter based on the predicate")(list.filter(a => a % 2 == 0) should equal(Node(2, Node(4, Empty))))
        it("should be equivalent to List") {
          list.filter(a => a % 2 == 0).toList should equal(List[Int](1, 2, 3, 4).filter(a => a % 2 == 0))
        }
      }
    }

    describe("find - Finds the first element of the sequence satisfying a predicate, if any.") {
      describe("When Empty") {
        it("should return none")(LinkedList().find(_ == 5) shouldBe None)
        it("should be equivalent to List")(LinkedList().find(_ == 5) shouldBe List().find(_ == 5))
      }
      describe("with one element") {
        list = LinkedList(3)
        it("should return the element found")(list.find(_ == 3) shouldBe Some(3))
        it("should return none")(list.find(_ == 5) shouldBe None)
        it("should be equivalent to List") {
          LinkedList().find(_ == 3) shouldBe List().find(_ == 3)
          LinkedList().find(_ == 5) shouldBe List().find(_ == 5)
        }
      }
      describe("with multiple elements") {
        list = LinkedList(1, 2, 3, 4, 5, 6)
        it("should return the element found")(list.find(_ == 5) shouldBe Some(5))
        it("should return none")(list.find(_ == -1) shouldBe None)
        it("should be equivalent to List") {
          LinkedList().find(_ == 5) shouldBe List().find(_ == 5)
          LinkedList().find(_ == -1) shouldBe List().find(_ == -1)
        }
      }
    }

    describe("map - Builds a new collection by applying a function to all elements of this list.") {
      describe("When Empty") {
        list = LinkedList[Int]()
        it("should return Empty")(list.map(a => a * a) shouldBe Empty)
        it("should be equivalent to List")(list.map(a => a * a).toList shouldBe List[Int]().map(a => a * a))
      }
      describe("with one element") {
        list = LinkedList(3)
        it("should square the element")(list.map(a => a * a) shouldBe Node(9, Empty))
        it("should be equivalent to List")(list.map(a => a * a).toList shouldBe List[Int](3).map(a => a * a))
      }
      describe("with multiple elements") {
        list = LinkedList(1, 2, 3)
        it("should square the element")(list.map(a => a * a) shouldBe Node(1, Node(4, Node(9, Empty))))
        it("should be equivalent to List")(list.map(a => a * a).toList shouldBe List[Int](1, 2, 3).map(a => a * a))
      }
    }

    describe("flatten - Converts this list of traversable collections into a list formed by the elements of " +
      "these traversable collections.") {
      describe("When Empty") {
        it("should return empty")(LinkedList().flatten shouldBe Empty)
        it("should be equivalent to List")(LinkedList().flatten.toList shouldBe List().flatten)
      }
      describe("with one element") {
        val list2 = LinkedList(LinkedList(1, 2, 3))
        it("should flatten the list within a list into one list")(list2.flatten shouldBe Node(1, Node(2, Node(3, Empty))))
        it("should be equivalent to List") {
          list2.flatten.toList shouldBe List(List(1, 2, 3)).flatten
        }
      }
      describe("with multiple elements") {
        val list2 = LinkedList(LinkedList(1, 2, 3), LinkedList(1, 2, 3), LinkedList(1, 2, 3))
        it("should flatten the lists within a list into one list") {
          list2.flatten shouldBe Node(1, Node(2, Node(3, Node(1, Node(2, Node(3, Node(1, Node(2, Node(3, Empty)))))))))
        }
        it("should be equivalent to List") {
          list2.flatten.toList shouldBe List(List(1, 2, 3), List(1, 2, 3), List(1, 2, 3)).flatten
        }
      }
    }

    describe("flatMap - Builds a new collection by applying a function to all elements of this list and using " +
      "the elements of the resulting collections.") {
      describe("When Empty") {
        it("should return empty")(LinkedList[LinkedList[Int]]().flatMap(x => x.map(y => y * 2)) shouldBe Empty)
        it("should be equivalent to List") {
          LinkedList[LinkedList[Int]]().flatMap(x => x.map(y => y * 2)).toList shouldBe
            List[List[Int]]().flatMap(x => x.map(y => y * 2))
        }
      }
      describe("with one element") {
        val list2 = LinkedList[LinkedList[Int]](LinkedList(1, 2, 3))
        it("should flatten the list within a list into one list") {
          list2.flatMap(x => x.map(y => y * 2)) shouldBe Node(2, Node(4, Node(6, Empty)))
        }
        it("should be equivalent to List") {
          list2.flatMap(x => x.map(y => y * 2)).toList shouldBe
            List[List[Int]](List(1, 2, 3)).flatMap(x => x.map(y => y * 2))
        }
      }
      describe("with multiple elements") {
        val list2 = LinkedList(LinkedList(1, 2, 3), LinkedList(1, 2, 3), LinkedList(1, 2, 3))
        it("should flatten the lists within a list into one list") {
          list2.flatMap(x => x.map(y => y * 2))
            .shouldBe(Node(2, Node(4, Node(6, Node(2, Node(4, Node(6, Node(2, Node(4, Node(6, Empty))))))))))
        }
        it("should be equivalent to List") {
          list2.flatMap(x => x.map(y => y * 2)).toList shouldBe
            List[List[Int]](List(1, 2, 3), List(1, 2, 3), List(1, 2, 3)).flatMap(x => x.map(y => y * 2))
        }
      }
    }

    describe("negate Negates the boolean operator of a predicate") {
      list = LinkedList(1)
      val f = list.negate(x => x == x)
      it("should negate a predicate")(f(1) shouldBe false)
    }

    describe("partition - Partitions this traversable collection in two traversable collections " +
      "according to a predicate.") {
      describe("When Empty") {
        list = LinkedList()
        it("should create two empty lists")(list.partition(x => x % 2 == 0) should equal((Empty, Empty)))
        it("should be equivalent to List") {
          val mine = list.partition(x => x % 2 == 0)
          val theres = List[Int]().partition(x => x % 2 == 0)

          mine._1.toList should equal(theres._1)
          mine._2.toList should equal(theres._2)
        }
      }
      describe("with one element") {
        list = LinkedList(4)
        it("should create two lists")(list.partition(x => x % 2 == 0) should equal((LinkedList(4), Empty)))
        it("should be equivalent to List") {
          val mine = list.partition(x => x % 2 == 0)
          val theres = List[Int](4).partition(x => x % 2 == 0)

          mine._1.toList should equal(theres._1)
          mine._2.toList should equal(theres._2)
        }
      }
      describe("with multiple elements") {
        list = LinkedList(1, 2, 3, 6, 7)
        it("should create two lists") {
          list.partition(x => x % 2 == 0) should equal((LinkedList(2, 6), LinkedList(1, 3, 7)))
        }
        it("should be equivalent to List") {
          val mine = list.partition(x => x % 2 == 0)
          val theres = List[Int](1, 2, 3, 6, 7).partition(x => x % 2 == 0)

          mine._1.toList should equal(theres._1)
          mine._2.toList should equal(theres._2)
        }
      }
    }

    describe("zip - Returns a list formed from this list and another iterable collection by combining " +
      "corresponding elements in pairs.") {
      describe("When Empty") {
        list = LinkedList()
        it("should return an empty list") {
          list zip LinkedList() should equal(Empty)
          list zip LinkedList(1, 2, 3) should equal(Empty)
        }
        it("should be equivalent to List") {
          (list zip LinkedList()).toList should equal(List() zip List())
          (list zip LinkedList(1, 2, 3)).toList should equal(List() zip List(1, 2, 3))
        }
      }
      describe("with one element") {
        list = LinkedList(4)
        it("should return an empty list")(list zip LinkedList() should equal(Empty))
        it("should return a list with one tuple")(list zip LinkedList(1, 2, 3) should equal(Node((4, 1), Empty)))
        it("should be equivalent to List") {
          (list zip LinkedList()).toList should equal(List(4) zip List())
          (list zip LinkedList(1, 2, 3)).toList should equal(List(4) zip List(1, 2, 3))
        }
      }
      describe("with multiple elements") {
        list = LinkedList(1, 2, 3, 6, 7)
        it("should return an empty list")(list zip LinkedList() should equal(Empty))
        it("should return a list of tuples") {
          list zip LinkedList(1, 2, 3) should equal(Node((1, 1), Node((2, 2), Node((3, 3), Empty))))
          list zip LinkedList(1, 2, 3, 4, 5) should equal {
            Node((1, 1), Node((2, 2), Node((3, 3), Node((6, 4), Node((7, 5), Empty)))))
          }
          list zip LinkedList(1, 2, 3, 4, 5, 6, 7, 8) should equal {
            Node((1, 1), Node((2, 2), Node((3, 3), Node((6, 4), Node((7, 5), Empty)))))
          }
        }
        it("should be equivalent to List") {
          (list zip LinkedList(1, 2, 3)).toList should equal(List(1, 2, 3, 6, 7) zip List(1, 2, 3))
          (list zip LinkedList(1, 2, 3, 4, 5)).toList should equal(List(1, 2, 3, 6, 7) zip List(1, 2, 3, 4, 5))
          (list zip LinkedList(1, 2, 3, 4, 5, 6, 7, 8)).toList should equal {
            List(1, 2, 3, 6, 7) zip List(1, 2, 3, 4, 5, 6, 7, 8)
          }
        }
      }
    }

    describe("collect - Builds a new collection by applying a partial function to all elements of this " +
      "list on which the function is defined.") {
      var list: LinkedList[Any] = null
      describe("When Empty") {
        list = LinkedList()
        it("should collect nothing")(list.collect { case i: Int => i + 1 } should equal(Empty))
        it("should be equivalent to List") {
          list.collect { case i: Int => i + 1 }.toList should equal(List[Any]().collect { case i: Int ⇒ i + 1 })
        }
      }
      describe("with one element") {
        list = LinkedList(4)
        val list2 = LinkedList[Any]("cat")
        it("should get and perform the function on the element") {
          list.collect { case i: Int => i + 1 } should equal(Node(5, Empty))
          list2.collect { case i: String => i + 1 } should equal(Node("cat1", Empty))
        }
        it("should not get and perform the function on the element") {
          list.collect { case i: String => i + 1 } should equal(Empty)
          list2.collect { case i: Int => i + 1 } should equal(Empty)
        }
        it("should be equivalent to List") {
          list.collect { case i: Int => i + 1 }.toList should
            equal(List[Any](4).collect { case i: Int => i + 1 })
          list.collect { case i: String => i + 1 }.toList should
            equal(List[Any](4).collect { case i: String => i + 1 })
          list2.collect { case i: Int => i + 1 }.toList should
            equal(List[Any]("cat").collect { case i: Int => i + 1 })
          list2.collect { case i: String => i + 1 }.toList should
            equal(List[Any]("cat").collect { case i: String => i + 1 })
        }
      }
      describe("with multiple elements") {
        list = LinkedList(1, "cat", 3, "dog", 7)
        it("should get and perform the function on the element") {
          list.collect { case i: Int => i + 1 } should equal(Node(2, Node(4, Node(8, Empty))))
          list.collect { case i: String => i + 1 } should equal(Node("cat1", Node("dog1", Empty)))
        }
        it("should be equivalent to List") {
          list.collect { case i: Int => i + 1 }.toList should
            equal(List[Any](1, "cat", 3, "dog", 7).collect { case i: Int => i + 1 })
          list.collect { case i: String => i + 1 }.toList should
            equal(List[Any](1, "cat", 3, "dog", 7).collect { case i: String => i + 1 })
        }
      }
    }

    describe("groupBy - Partitions this traversable collection into a map of traversable collections according " +
      "to some discriminator function.") {
      var list: LinkedList[String] = null
      describe("When Empty") {
        list = LinkedList()
        it("should return an empty map")(list.groupBy(_.charAt(0)) should equal(Map()))
        it("should be equivalent to List")(list.groupBy(_.charAt(0)) should equal(List[String]().groupBy(_.charAt(0))))
      }
      describe("with one element") {
        list = LinkedList("Golden Eagle")
        it("should return a map with one key and one list") {
          list.groupBy(_.charAt(0)) should equal(Map('G' -> Node("Golden Eagle", Empty)))
        }
        it("should be equivalent to List") {
          val list2 = List("Golden Eagle").groupBy(_.charAt(0))
          list.groupBy(_.charAt(0)).foreach((t) => t._2.toList should equal(list2.get(t._1).get))
        }
      }
      describe("with multiple elements") {
        list = LinkedList("Golden Eagle", "Gyrfalcon", "American Robin", "Mountain BlueBird", "Mountain-Hawk Eagle")
        it("should return a map with grouped elements") {
          list.groupBy(_.charAt(0)) should
            equal(Map(
              'M' -> Node("Mountain BlueBird", Node("Mountain-Hawk Eagle", Empty)),
              'G' -> Node("Golden Eagle", Node("Gyrfalcon", Empty)),
              'A' -> Node("American Robin", Empty)
            ))
        }
        it("should be equivalent to List") {
          val list2 = List("Golden Eagle", "Gyrfalcon", "American Robin", "Mountain BlueBird", "Mountain-Hawk Eagle")
            .groupBy(_.charAt(0))
          list.groupBy(_.charAt(0)).foreach((t) => t._2.toList should equal(list2.get(t._1).get))
          list.groupBy(_.charAt(0)).size should equal(list2.size)
        }
      }
    }

    describe("splitAt - Splits this list into two at a given position. " +
      "Note: c splitAt n is equivalent to (but possibly more efficient than) (c take n, c drop n).") {
      describe("When Empty") {
        list = LinkedList()
        it("should return a tuple of empty")(list.splitAt(5) should equal((Empty, Empty)))
        it("should be equivalent to List") {
          val l = list.splitAt(5)
          val l2 = List().splitAt(5)
          l._1.toList should equal(l2._1)
          l._2.toList should equal(l2._2)
        }
      }
      describe("with one element") {
        list = LinkedList(4)
        it("should return a tuple of of node and empty")(list.splitAt(5) should equal((Node(4, Empty), Empty)))
        it("should return a tuple of of empty and node")(list.splitAt(0) should equal(Empty, Node(4, Empty)))
        it("should be equivalent to List") {
          val l = list.splitAt(3)
          val l2 = List(4).splitAt(3)
          l._1.toList should equal(l2._1)
          l._2.toList should equal(l2._2)

          val l3 = list.splitAt(0)
          val l4 = List(4).splitAt(0)
          l3._1.toList should equal(l4._1)
          l3._2.toList should equal(l4._2)
        }
      }
      describe("with multiple elements") {
        list = LinkedList(1, 2, 3, 6, 7)
        it("should return a tuple of of node and empty") {
          list.splitAt(3) should equal((Node(1, Node(2, Node(3, Empty))), Node(6, Node(7, Empty))))
        }
        it("should be equivalent to List") {
          val l = list.splitAt(3)
          val l2 = List(1, 2, 3, 6, 7).splitAt(3)
          l._1.toList should equal(l2._1)
          l._2.toList should equal(l2._2)
        }
      }
    }

    describe("take - Selects first n elements.") {
      describe("When Empty") {
        list = LinkedList()
        it("should return empty")(list.take(4) should equal(Empty))
        it("should be equivalent to List")(list.take(4).toList should equal(List().take(4)))
      }
      describe("with one element") {
        list = LinkedList(4)
        it("should return one element list")(list.take(5) should equal(Node(4, Empty)))
        it("should be equivalent to List") (list.take(5).toList should equal(List(4).take(5)))
      }
      describe("with multiple elements") {
        list = LinkedList(1, 2, 3, 6, 7)
        val a = list.take(3)
        it("should return the number of elements")(list.take(3) should equal(Node(1, Node(2, Node(3, Empty)))))
        it("should be equivalent to List") (list.take(3).toList should equal(List(1, 2, 3, 6, 7).take(3)))
      }
    }

    describe("drop - Selects all elements except first n ones.") {
      describe("When Empty") {
        list = LinkedList()
        it("should return empty")(list.drop(4) should equal(Empty))
        it("should be equivalent to List")(list.drop(4).toList should equal(List().drop(4)))
      }
      describe("with one element") {
        list = LinkedList(4)
        it("should return one element list")(list.drop(5) should equal(Empty))
        it("should be equivalent to List") (list.drop(5).toList should equal(List(4).drop(5)))
      }
      describe("with multiple elements") {
        list = LinkedList(1, 2, 3, 6, 7)
        val a = list.take(3)
        it("should return the number of elements")(list.drop(3) should equal(Node(6, Node(7, Empty))))
        it("should be equivalent to List") (list.drop(3).toList should equal(List(1, 2, 3, 6, 7).drop(3)))
      }
    }

    describe("last - Selects the last element.") {
      describe("When Empty") {
        list = LinkedList()
        it("should return throw an exception")(intercept[NoSuchElementException](list.last))
        it("should be equivalent to List")(intercept[NoSuchElementException](List().last))
      }
      describe("with one element") {
        list = LinkedList(4)
        it("should return the head")(list.last should equal(list.head))
        it("should be equivalent to List") {
          list.last should equal(List(4).head)
          list.last should equal(List(4).last)
        }
      }
      describe("with multiple elements") {
        list = LinkedList(1, 2, 3, 6, 7)
        it("should return the head")(list.last should equal(7))
        it("should be equivalent to List")(list.last should equal(List(1, 2, 3, 6, 7).last))
      }
    }

    describe("+: - A copy of the list with an element prepended.") {
      describe("When Empty") {
        list = LinkedList()
        it("should prepend one element")(list.+:(3) should equal(Node(3, Empty)))
        it("should be equivalent to List")(list.+:(3).toList should equal(List().::(3)))
      }
      describe("with one element") {
        list = LinkedList(4)
        it("should prepend one element")(list.+:(3) should equal(Node(3, Node(4, Empty))))
        it("should be equivalent to List")(list.+:(3).toList should equal(List(4).+:(3)))
      }
      describe("with multiple elements") {
        list = LinkedList(4, 5, 6, 7)
        it("should prepend one element")(list.+:(3) should equal(Node(3, Node(4, Node(5, Node(6, Node(7, Empty)))))))
        it("should be equivalent to List")((3 +: 1 +: 2 +: Empty).toList should equal(List(3, 1, 2)))
      }
      it("should be right associative") {
        (3 +: 1 +: 2 +: Empty) should equal(Node(3, Node(1, Node(2, Empty))))
      }
    }

    describe("::: - Adds the elements of a given list in front of this list.") {
      val listToBAdded = LinkedList(1, 2, 3)
      describe("When Empty") {
        list = LinkedList()
        it("should take the form of the added list")((listToBAdded ::: list) should equal(listToBAdded))
        it("should be empty if both lists are empty")((LinkedList() ::: list) should equal(Empty))
        it("should be equivalent to List") {
          (listToBAdded ::: list).toList should equal(List(1, 2, 3) ::: List())
          (LinkedList() ::: list).toList should equal(List() ::: List())
        }
      }
      describe("with one element") {
        list = LinkedList(3)
        it("should take the form of the original list if added list is empty") {
          (LinkedList() ::: list) should equal(list)
        }
        it("should add the two lists together")((listToBAdded ::: list) should equal(LinkedList(1, 2, 3, 3)))
        it("should be equivalent to List") {
          (listToBAdded ::: list).toList should equal(List(1, 2, 3) ::: List(3))
          (LinkedList() ::: list).toList should equal(List() ::: List(3))
        }
      }
      describe("with multiple elements") {
        list = LinkedList(3, 9, 5, 7)
        it("should add the two lists together")((listToBAdded ::: list) should equal(LinkedList(1, 2, 3, 3, 9, 5, 7)))
        it("should be equivalent to List") {
          (listToBAdded ::: list).toList should equal(List(1, 2, 3) ::: List(3, 9, 5, 7))
        }
      }
    }

    describe("sum") {
      describe("When Empty") {
        list = LinkedList[Int]()
        it("should be zero") (list.sum should equal(0))
        it("should be equivalent to List")(list.sum should equal(List[Int]().sum))
      }
      describe("with one element") {
        list = LinkedList(4)
        it("should be 4")(list.sum should equal(4))
        it("should be equivalent to List")(list.sum should equal(List(4).sum))
      }
      describe("with multiple elements") {
        list = LinkedList(1, 2, 3, 6, 7)
        it("should be 4")(list.sum should equal(19))
        it("should be equivalent to List")(list.sum should equal(List(1, 2, 3, 6, 7).sum))
      }
    }

    describe("max") {
      describe("When Empty") {
        list = LinkedList[Int]()
        it("should throw an exception")(intercept[UnsupportedOperationException](list.max))
        it("should be equivalent to List")(intercept[UnsupportedOperationException](List().max))
      }
      describe("with one element") {
        list = LinkedList(4)
        it("should return the head")(list.max should equal(list.head))
        it("should be equivalent to List")(list.max should equal(List(4).max))
      }
      describe("with multiple elements") {
        list = LinkedList(1, 2, 8, 6, 7)
        it("should return the max value")(list.max should equal(8))
        it("should be equivalent to List")(list.max should equal(List(1, 2, 8, 6, 7).max))
      }
    }

    describe("min") {
      describe("When Empty") {
        list = LinkedList[Int]()
        it("should throw an exception")(intercept[UnsupportedOperationException](list.min))
        it("should be equivalent to List")(intercept[UnsupportedOperationException](List().min))
      }
      describe("with one element") {
        list = LinkedList(4)
        it("should return the head")(list.min should equal(list.head))
        it("should be equivalent to List")(list.min should equal(List(4).min))
      }
      describe("with multiple elements") {
        list = LinkedList(1, 2, 8, 6, 7)
        it("should return the max value")(list.min should equal(1))
        it("should be equivalent to List")(list.min should equal(List(1, 2, 8, 6, 7).min))
      }
    }

    describe("mergeSort") {
      describe("When Empty") {
        list = LinkedList()
        it("should return an empty")(list.mergeSort should equal(Empty))
      }
      describe("with one element") {
        list = LinkedList(4)
        it("should return an empty")(list.mergeSort should equal(Node(4, Empty)))
      }
      describe("with multiple elements") {
        list = LinkedList(5, 6, 2, 4, 3, 1)
        it("should return an empty") {
          list.mergeSort should equal(Node(1, Node(2, Node(3, Node(4, Node(5, Node(6, Empty)))))))
        }
      }
    }
  }
}
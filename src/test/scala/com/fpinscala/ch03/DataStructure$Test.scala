package com.fpinscala.ch03

import org.scalatest.FlatSpec
import DataStructure._

/**
  * Created by wqlin on 17-10-30 19:53.
  */
class DataStructure$Test extends FlatSpec {
  "tail" should "throw Error" in {
    assertThrows[Error](tail(Nil))
  }

  "tail" should "return tail" in {
    assert(tail(List(1, 2, 3, 4)) == List(2, 3, 4))
  }

  "setHead" should "return a list with new head" in {
    assert(setHead(List(1, 2, 3, 4, 5), 10) == List(10, 2, 3, 4, 5))
  }

  "drop" should "drop 3 element" in {
    assert(drop(List(1, 2, 3, 4, 5), 3) == List(4, 5))
  }

  "dropWhile" should "drop prefix element less than 0" in {
    assert(dropWhile[Int](List(-1, -2, -3, -4, -5), _ < 0) == List())
  }

  "init" should "drop the last element of list" in {
    assert(init(List(1, 2, 3, 4)) == List(1, 2, 3))
  }

  "init" should "throw error for empty list" in {
    assertThrows[Error](init(Nil))
  }

  "length" should "return the length of list" in {
    assert(length(List(1, 2, 3)) == 3)
    assert(length(Nil) == 0)
  }

  "foldLeft" should "return the sum of list" in {
    assert(foldLeft(List(1, 2, 3), 0)(_ + _) == 6)
  }

  "sum" should "sum the element of list" in {
    assert(sum(List(10, 9, 8)) == 27)
  }

  "product" should "multiply the element of list" in {
    assert((product(List(1.0, 2.0, 3.0)) - 6.0).abs < 0.001)
  }

  "reverse" should "reverse list" in {
    assert(reverse(List('a, 'b, 'c, 'd)) == List('d, 'c, 'b, 'a))
  }

  "append" should "append two list" in {
    assert(append(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6))
    assert(append(List(), List(1, 2, 3)) == List(1, 2, 3))
    assert(append(List(1, 2, 3), List()) == List(1, 2, 3))
  }

  "concatAll" should "concatenate all list" in {
    assert(concatAll(List(List('H, 'e, 'l), List('l, 'l, 'o, 'w), List('o, 'r, 'l, 'd))) == List('H, 'e, 'l, 'l, 'l, 'o, 'w, 'o, 'r, 'l, 'd))
  }

  "plusOne" should "increment element by one" in {
    assert(plusOne(List(1, 2, 3)) == List(2, 3, 4))
  }

  "DoubleToString" should "convert all double to string" in {
    assert(DoubleToString(List(1.0, 2.0, 3.0)) == List("1.0", "2.0", "3.0"))
  }

  "filter" should "filter out all odd number" in {
    assert(filter(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(_ % 2 == 0) == List(2, 4, 6, 8, 10))
  }

  "flatMap" should "flatten list" in {
    assert(flatMap(List(1, 2, 3))(i => List(i, i)) == List(1, 1, 2, 2, 3, 3))
  }

  "filterViaFlatMap" should "filter out add even number" in {
    assert(filterViaFlatMap(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(_ % 2 != 0) == List(1, 3, 5, 7, 9))
  }

  "addPair" should "return a new list" in {
    assert(addPair(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))
    assert(addPair(List(1, 2, 3), List(4)) == List(5))
    assert(addPair(List(3), List(5, 6)) == List(8))
  }

  "hasSubsequence" should "check whether a list has a specific sub sequence" in {
    assert(hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
    assert(!hasSubsequence(List('a, 'b, 'c, 'd), List('d, 'e)))
    assert(hasSubsequence(List('a, 'b, 'c, 'd), Nil))
  }

  "size" should "return the size of a tree" in {
    assert(size(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) == 7)
  }

  "maximum" should "return the maximum element" in {
    assert(maximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) == 4)
  }

  "depth" should "return the maximum depth of a tree" in {
    assert(depth(Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(4), Branch(Leaf(5), Branch(Leaf(6), Leaf(7))))))) == 6)
  }

  "map" should "apply function to every element and return a new tree" in {
    assert(map(Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(4), Branch(Leaf(5), Branch(Leaf(6), Leaf(7)))))))(a => a + 1) ==
      Branch(Leaf(2), Branch(Branch(Leaf(3), Leaf(4)), Branch(Leaf(5), Branch(Leaf(6), Branch(Leaf(7), Leaf(8)))))))
  }
}

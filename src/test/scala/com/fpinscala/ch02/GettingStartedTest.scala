package com.fpinscala.ch02

import org.scalatest.FlatSpec
import GettingStarted._

/**
  * Created by wqlin on 17-10-30 18:57.
  */
class GettingStartedTest extends FlatSpec {
  // Test for exercise 2.1
  "fib" should "return the right answer for ith Fibonacci number" in {
    assert(fib(0) == 0)
    assert(fib(1) == 1)
    assert(fib(7) == 13)
    assert(fib(13) == 233)
  }

  // Test for exercise 2.2
  "isSorted" should "return false" in {
    assert(!isSorted[Int](Array(1, 2, 3, 4, 5, 100, 7, 8, 9, 10, 11, 12, 13, 14), _ < _))
  }

  "isSorted" should "return true" in {
    assert(isSorted[Int](Array(100, 90, 80, 70, 60, 50), _ > _))
  }
}

package com.fpinscala.ch02

import org.scalatest.FlatSpec
import GettingStarted._

/**
  * Created by wqlin on 17-10-30 18:57.
  */
class GettingStarted$Test extends FlatSpec {
  // Test for exercise 2.1
  "fib" should "return the right answer for ith Fibonacci number" in {
    assert(fib(0) == 0)
    assert(fib(1) == 1)
    assert(fib(7) == 13)
    assert(fib(13) == 233)
  }

  // Test for exercise 2.2
  "isSorted" should "return false" in {
    assert(!isSorted[Int](Array(1, 3, 2, 4, 5, 6), _ < _))
  }

  "isSorted" should "return true" in {
    assert(isSorted[Int](Array(100, 90, 80, 70, 60, 50), _ > _))
  }
}

package com.fpinscala.ch04

import org.scalatest.FlatSpec
import ErrorHandling._

/**
  * Created by wqlin on 17-10-31 10:56.
  */
class ErrorHandlingTest extends FlatSpec {
  "variance" should "compute the right variance" in {
    assert(variance(Array(11.0, 12.0, 13.0, 14.0, 15.0)) == Some(2.0))
  }

  "sequence" should "return a option of list" in {
    //assert(sequence(List(Some(1), Some(2), None)) == None)
    //assert(sequence(List(Some(1), Some(2), Some(3), Some(4), Some(5))) == Some(List(1, 2, 3, 4, 5)))
  }
}

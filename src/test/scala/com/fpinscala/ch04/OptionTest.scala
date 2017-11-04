package com.fpinscala.ch04

import org.scalatest.FlatSpec
import scala.{Option => _, Either => _, Some => _, None => _}
import Option._

/**
  * Created by wqlin on 17-10-31 10:56.
  */
class OptionTest extends FlatSpec {
  "variance" should "compute the right variance" in {
    assert(variance(Array(11.0, 12.0, 13.0, 14.0, 15.0)) == Some(2.0))
  }

  "sequence" should "return a option of list" in {
    assert(sequence(List(Some(1), Some(2), None, Some(4), Some(5))) == None)
    assert(sequence(List(Some(1), Some(2), Some(3), Some(4), Some(5))) == Some(List(1, 2, 3, 4, 5)))
    assert(sequence(List(Some('a), Some('b), Some('c), Some('d))) == Some(List('a, 'b, 'c, 'd)))
    assert(sequence(List(None)) == None)
    assert(sequence(List(Some('a))) == Some(List('a)))
  }

  "sequenceViaTraverse" should "return a option of list" in {
    assert(sequenceViaTraverse(List(Some(1), Some(2), None, Some(4), Some(5))) == None)
    assert(sequenceViaTraverse(List(Some(1), Some(2), Some(3), Some(4), Some(5))) == Some(List(1, 2, 3, 4, 5)))
    assert(sequenceViaTraverse(List(Some('a), Some('b), Some('c), Some('d))) == Some(List('a, 'b, 'c, 'd)))
    assert(sequenceViaTraverse(List(None)) == None)
    assert(sequenceViaTraverse(List(Some('a))) == Some(List('a)))
  }

  "traverse" should "return a option of list" in {
    assert(traverse(List("-1", "-2", "-3", "-4"))(s => Try(s.toInt)) == Some(List(-1, -2, -3, -4)))
    assert(traverse(List("1", "2", "3", "4"))(s => Try(s.toInt)) == Some(List(1, 2, 3, 4)))
    assert(traverse(List("-1", "-2", "k", "-5"))(s => Try(s.toInt)) == None)
  }

}

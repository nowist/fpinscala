package com.fpinscala.ch04

import org.scalatest.FlatSpec
import scala.{Option => _, Either => _, Left => _, Right => _}
import Either._

/**
  * Created by wqlin on 17-11-4 09:27.
  */
class EitherTest extends FlatSpec {
  "sequence" should "return a option of list" in {
    assert(sequence(List(Right(1), Right(2), Left(Nil), Right(4), Right(5))) == Left(Nil))
    assert(sequence(List(Right(1), Right(2), Right(3), Right(4), Right(5))) == Right(List(1, 2, 3, 4, 5)))
    assert(sequence(List(Left(Nil), Right('a), Right('b), Right('c), Right('d))) == Left(Nil))
    assert(sequence(List(Left(None))) == Left(None))
    assert(sequence(List(Right('a))) == Right(List('a)))
  }

  "traverse" should "return a option of list" in {
    assert(traverse(List("-1", "-2", "-3", "-4"))(s => Try(s.toInt)) == Right(List(-1, -2, -3, -4)))
    assert(traverse(List("1", "2", "3", "4"))(s => Try(s.toInt)) == Right(List(1, 2, 3, 4)))
    assert(traverse(List("-1", "-2", "k", "-5"))(s => Try(s.toInt)).orElse(Left(Nil)) == Left(Nil))
  }
}

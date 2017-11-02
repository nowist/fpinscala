package com.fpinscala.ch05

import org.scalatest.FlatSpec
import scala.collection.immutable.{Stream => _}

/**
  * Created by wqlin on 17-10-31 15:14.
  */
class StreamTest extends FlatSpec {
  val s = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  val s1 = Stream(2, 4, 6, 8, 10, 11, 12)
  "take" should "take element" in {
    assert(s.take(3).toList == List(1, 2, 3))
    assert(Stream().take(1).toList == List())
    assert(s.take(100).toList == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    assert(s.takeViaUnfold(3).toList == List(1, 2, 3))
    assert(Stream().takeViaUnfold(1).toList == List())
    assert(s.takeViaUnfold(100).toList == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  }
  "drop" should "drop first 5 element" in {
    assert(s.drop(5).toList == List(6, 7, 8, 9, 10))
  }

  "takeWhile" should "take all element less than 12" in {
    assert(s.takeWhile(_ < 12).toList == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  }

  "forall" should "return true" in {
    assert(s.forAll(_ <= 10))
    assert(Stream[Int]().forAll(_ > 10))
  }

  "takeWhileViaFoldRight" should "take element" in {
    assert(s.takeWhileViaFoldRight(_ % 2 != 0).toList == List(1))
    assert(s1.takeWhileViaFoldRight(_ % 2 == 0).toList == List(2, 4, 6, 8, 10))
  }

  "takeWhileViaUnfold" should "take element" in {
    assert(s.takeWhileViaUnfold(_ % 2 != 0).toList == List(1))
    assert(s1.takeWhileViaUnfold(_ % 2 == 0).toList == List(2, 4, 6, 8, 10))
  }

  "headOption" should "return head option" in {
    assert(s.headOption.contains(1))
    assert(s1.headOption.contains(2))
    assert(Stream().headOption.isEmpty)
  }

  "map" should "convert element to string" in {
    assert(s.map(_.toString).toList == List("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
    assert(s.mapViaUnfold(_.toString).toList == List("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
  }

  "filter" should "filter out odd number" in {
    assert(s1.filter(_ % 2 == 0).toList == List(2, 4, 6, 8, 10, 12))
  }

  "append" should "concatenate two list" in {
    assert((s append s1).toList == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 2, 4, 6, 8, 10, 11, 12))
  }

  "flatMap" should "map and flatten list" in {
    assert(s1.flatMap(x => Stream(x, x)).toList == List(2, 2, 4, 4, 6, 6, 8, 8, 10, 10, 11, 11, 12, 12))
  }

  "constant" should "produce infinite constant stream" in {
    assert(Stream.constant(10).take(10).toList == List.fill(10)(10))
    assert(Stream.constantViaUnfold(5).take(10).toList == List.fill(10)(5))
  }

  "from" should "produce infinite increasing stream" in {
    assert(Stream.from(100).take(5).toList == List(100, 101, 102, 103, 104))
    assert(Stream.fromViaUnfold(10).take(5).toList == List(10, 11, 12, 13, 14))
  }

  "fib" should "produce infinite fibonacci number" in {
    assert(Stream.fibs().take(10).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
    assert(Stream.fibsViaUnfold().take(10).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
  }

  "ones" should "produce infinite one" in {
    assert(Stream.onesViaUnfold().take(10).toList == List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
  }

  "tails" should "produce all postfix stream" in {
    assert(Stream(1, 2, 3).tails.map(_.toList).toList == List(List(1, 2, 3), List(2, 3), List(3), List()))
  }

  "zipWith" should "zip another stream" in {
    assert(s.zipWith(s1)((a, b) => a + b).toList == List(3, 6, 9, 12, 15, 17, 19))
  }

  "zipAll" should "zip stream together" in {
    assert((s zipAll s1).toList == List((Some(1), Some(2)), (Some(2), Some(4)), (Some(3), Some(6)), (Some(4), Some(8)), (Some(5), Some(10)), (Some(6), Some(11)), (Some(7), Some(12)), (Some(8), None), (Some(9), None), (Some(10), None)))
  }

  "startsWith" should "check whether a stream starts with another stream" in {
    assert(s startsWith Stream(1, 2, 3))
    assert(s startsWith Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    assert(!(s startsWith Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)))
    assert(!(s startsWith Stream.constant(10)))
  }
}

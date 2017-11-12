package com.fpinscala.ch10

import java.util.concurrent.Executors

import org.scalatest.FlatSpec
import Monoid._
import com.fpinscala.ch08.{Gen, Prop}

/**
  * Created by wqlin on 17-11-10 16:33.
  */
class MonoidTest extends FlatSpec {
  val intGen = Gen.choose(Int.MinValue, Int.MaxValue)
  val booleanGen = Gen.boolean

  "monoid zero" should "satisfy associative law" in {

    val intAdditionProp = monoidLaws(intAddition, intGen)
    Prop.run(intAdditionProp, testCases = 500)
    val intMultiplicationProp = monoidLaws(intMultiplication, intGen)
    Prop.run(intMultiplicationProp, testCases = 500)

    val booleanOrProp = monoidLaws(booleanOr, booleanGen)
    Prop.run(booleanOrProp, testCases = 500)
    val booleanAndProp = monoidLaws(booleanAnd, booleanGen)
    Prop.run(booleanAndProp, testCases = 500)
  }

  "endoMonoid" should "satisfy monoid laws" in {
    val op1: Int => Int = _ + 1
    val op2: Int => Int = _ * 2
    val op3: Int => Int = _ - 2

    val zeroRight = endoMonoid1.op(op1, endoMonoid1.zero)
    val zeroLeft = endoMonoid1.op(endoMonoid1.zero, op1)
    assert((0 to 100000).forall(x => zeroLeft(x) == zeroRight(x)))

    val f = endoMonoid1.op(endoMonoid1.op(op1, op2), op3)
    val g = endoMonoid1.op(op1, endoMonoid1.op(op2, op3))
    assert((0 to 100000).forall(x => f(x) == g(x)))

  }

  "foldMap" should "map and fold list" in {
    assert(foldMap(List(1, 2, 3, 4, 5), stringMonoid)(_.toString) == "12345")
    assert(foldMapViaFoldRight(List(1, 2, 3, 4, 5), stringMonoid)(_.toString) == "12345")
  }

  "foldLeft" should "fold list from left" in {
    assert(foldLeft(List('b', 'c', 'd', 'e'))("a")((s, c) => s + c.toString) == "abcde")
  }

  "foldRight" should "fold list from right" in {
    assert(foldRight(List('b', 'c', 'd', 'e'))("a")((c, s) => c.toString + s) == "bcdea")
  }

  "foldMapV" should "fold IndexedSeq" in {
    assert(foldMapV(1 to 10, intAddition)(_ * 2) == 110)
    assert(foldMapV(1 to 10, intMultiplication)(x => x) == 3628800)
  }

  "parfoldMap" should "fold IndexedSed in parallel" in {
    val es = Executors.newFixedThreadPool(8)
    assert(parFoldMap(1 to 10000000, intAddition)(x => x)(es).get == -2004260032)
  }

  //Exercise 10.9
  def isSorted(ls: List[Int]): Boolean =
    foldMap(ls zip ls.tail, booleanAnd) { case (i, j) => i < j }

  "isSorted" should "determine if list is sorted" in {
    assert(isSorted(List(1, 2, 3, 4, 5)))
    assert(!isSorted(List(1, 2, 5, 4, 6)))
    assert(!isSorted(List(10, 11, 14, 13)))
  }

  "wordCount" should "count words" in {
    assert(wordCount("wordCount should count words") == 4)
    assert(wordCount("hello world") == 2)
  }

  "bag" should "return a map of words and corresponding frequency" in {
    assert(bag(Vector("a", "rose", "a", "is", "rose")) == Map("a" -> 2, "rose" -> 2, "is" -> 1))
  }
}

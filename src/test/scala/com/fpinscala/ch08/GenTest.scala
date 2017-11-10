package com.fpinscala.ch08

import org.scalatest.FlatSpec
import com.fpinscala.ch06.RNG._
import com.fpinscala.ch06.{RNG, State}

/**
  * Created by wqlin on 17-11-8 14:05.
  */
class GenTest extends FlatSpec {
  val seed: RNG = Simple(System.nanoTime())
  val sample: State[RNG, Int] = State.unit(seed => seed.nextInt)
  val g = Gen(sample)
  //Exercise 8.13
  "listOf1" should "generate non empty list" in {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = Prop.forAll(Gen.listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    Prop.run(maxProp)
  }

  //Exercise 8.14
  "list.sorted" should "sort list" in {
    val smallInt = Gen.choose(-100, 100)
    val sortProp = Prop.forAll(Gen.listOf1(smallInt)) { ns =>
      val nns = ns.sorted
      (nns zip nns.tail).forall { case (i, j) => i <= j }
    }
    Prop.run(sortProp, testCases = 200)
  }
}

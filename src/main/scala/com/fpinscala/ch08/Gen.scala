package com.fpinscala.ch08


import java.util.concurrent.{ExecutorService, Executors}

import com.fpinscala.ch06.{RNG, State}

import scala.collection.immutable.{Stream => _}
import com.fpinscala.ch08.Prop.{FailedCase, SuccessCount}
import com.fpinscala.ch05.Stream
import Prop._
import Gen._
import com.fpinscala.ch07.Par
import com.fpinscala.ch07.Par.Par

/**
  * Created by wqlin on 17-11-8 10:35.
  */
//Prop is short for property
//Prop need to generate random test cases, so it needs an RNG
case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  //composing properties
  def &&(p: Prop): Prop = Prop {
    (m, n, r) =>
      run(m, n, r) match {
        case Passed | Proved => p.run(m, n, r)
        case f: Falsified => f
      }
  }

  def ||(p: Prop): Prop = Prop {
    (m, n, r) =>
      run(m, n, r) match {
        case Passed => Passed
        case Proved => Proved
        case _: Falsified => p.run(m, n, r)
      }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  //Indicates that all tests passes
  case object Passed extends Result {
    def isFalsified: Boolean = false
  }

  //Indicates that one of the test case falsified that property
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified: Boolean = true
  }

  case object Proved extends Result {
    def isFalsified: Boolean = true
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop = props.map(p => Prop { (max, _, rng) => p.run(max, casesPerSize, rng) }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (num, rng) =>
      //The zip operation will generate a stream of pairs (a,i)
      //where a is a random value and i is its index in the stream
      randomStream(as)(rng).zip(Stream.from(0)).take(num).map {
        case (a, i) => try {
          if (f(a)) Passed
          else Falsified(a.toString, i) // When a test fails, record the failed case and its index
        } catch {
          //If a test case generates an exception, record it in the result
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def apply(f: (TestCases, RNG) => Result): Prop =
    Prop { (_, n, rng) => f(n, rng) }

  //Generates an infinite stream of A values by repeatedly sampling a generator
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Proved => println(s"+ OK, proved property.")
    }

  //A combinator for checking condition p
  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified("()", 0)
  }

  //Testing parallel computations
  val ES: ExecutorService = Executors.newCachedThreadPool
  val p1 = Prop.forAll(Gen.unit(Par.unit(1)))(i =>
    Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)

  val p2 = Prop.check {
    val p = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    p(ES).get == p2(ES).get
  }

  //Lift the equality comparison into Par
  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)

  val p3 = check {
    equal(Par.map(Par.unit(1))(_ + 1), Par.unit(2))(ES).get
  }

  val S = weighted(choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25) // a -> b is the syntax sugar for (a,b)

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S.map2(g)((_, _))) { case (s, a) => f(a)(s).get } // s is an Executor

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  val pint = Gen.choose(0, 10).map(Par.unit(_))
  val p4 = forAllPar(pint)(n => equal(Par.map(n)(y => y), n))
}

//Gen is short for generator
object Gen {
  //Exercise 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(rng => {
    val (i, r) = RNG.nonNegativeInt(rng)
    (i % (stopExclusive - start) + start, r)
  }))

  //Exercise 8.5
  def unit[A](a: => A): Gen[A] = Gen(State.unit((a, _)))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  //generate n test cases
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  //Exercise 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = Gen(State(rng => {
    val (b, r) = RNG.boolean(rng)
    if (b)
      g1.sample.run(r)
    else
      g2.sample.run(r)
  }))

  //Exercise 8.8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = Gen(State(rng => {
    /* The probability we should pull from `g1`. */
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    val (d, r) = RNG.double(rng)
    if (d <= g1Threshold)
      g1._1.sample.run(r)
    else
      g2._1.sample.run(r)
  }))

  //Exercise 8.12
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(Gen.listOfN(_, g))

  //Exercise 8.13
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => Gen.listOfN(n max 1, g))

  //An extractor
  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }

}

case class Gen[+A](sample: State[RNG, A]) {
  //Exercise 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))

  //Another implementation
  //  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(State(rng => {
  //    val (a, r) = sample.run(rng)
  //    f(a).sample.run(r)
  //  }))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen(State.sequence(List.fill(n)(sample))))

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    this.flatMap(a => g.map(b => f(a, b)))

  //Exercise 8.10
  def unsized: SGen[A] = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A, B)] = (this map2 g) ((_, _))
}

case class SGen[+A](g: Int => Gen[A]) {
  //Exercise 8.11
  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] =
    SGen(g(_).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(n => {
    val ga: Gen[A] = g(n)
    ga.flatMap(f(_).g(n))
  })

  def **[B](s2: SGen[B]): SGen[(A, B)] = SGen(n => apply(n) ** s2(n))

}
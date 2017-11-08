package com.fpinscala.ch06

/**
  * Created by wqlin on 17-11-2 15:11.
  */
trait RNG {
  def nextInt: (Int, RNG) // generate a random number as well as a new RNG state
}

object RNG {

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def int: Rand[Int] = _.nextInt

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  //Exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nextInt, nextRNG) = rng.nextInt
    if (nextInt < 0) (-(nextInt + 1), nextRNG) else (nextInt, nextRNG)
  }

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match {
      case (i, rng2) => (i % 2 == 0, rng2)
    }

  //Exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (nextInt, nextRNG) = nonNegativeInt(rng)
    (nextInt / (Int.MaxValue.toDouble + 1), nextRNG)
  }

  //Exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (nextInt, nextRNG1) = rng.nextInt
    val (nextDouble, nextRNG2) = double(nextRNG1)
    ((nextInt, nextDouble), nextRNG2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (nextDouble, nextRNG1) = double(rng)
    val (nextInt, nextRNG2) = nextRNG1.nextInt
    ((nextDouble, nextInt), nextRNG2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (nextDouble1, nextRNG1) = double(rng)
    val (nextDouble2, nextRNG2) = double(nextRNG1)
    val (nextDouble3, nextRNG3) = double(nextRNG2)
    ((nextDouble1, nextDouble2, nextDouble3), nextRNG3)
  }

  //Exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (0 until count).foldLeft((List.empty[Int], rng)) {
      case ((ls, r), _) => val (nextInt, nextRNG) = r.nextInt
        (nextInt :: ls, nextRNG)
    }
  }

  //Tail recursive solution
  def intsTailRec(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(count: Int, rng: RNG, ls: List[Int]): (List[Int], RNG) =
      if (count == 0) (ls, rng)
      else {
        val (nextInt, nextRNG) = rng.nextInt
        loop(count - 1, nextRNG, nextInt :: ls)
      }

    loop(count, rng, Nil)
  }

  //Exercise 6.5
  def doubleViaMap(rng: RNG): Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  //Exercise 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  def randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  //Exercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(Nil: List[A]))((a, b) => map2(a, b)(_ :: _))

  //Trivial implementation
  //  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
  //    rng => fs.foldRight((List.empty[A], rng)) {
  //      case ((f, (ls, r))) =>
  //        val (i, nextRNG) = f(r)
  //        (i :: ls, nextRNG)
  //    }

  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    rng =>
      val (i, rng2) = nonNegativeInt(rng)
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        (mod, rng2)
      else
        nonNegativeLessThan(n)(rng2)
  }

  //Exercise 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r) = f(rng)
    g(a)(r)
  }

  def nonNegativeLessThanViaFlatMap(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThanViaFlatMap(n)
    }

  //Exercise 6.9
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

  //Instead, We could also use placeholder to replace unit function
  def mapViaFlatMap1[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => (f(a), _))

  def map2ViaFlatMap1[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => (f(a, b), _)))
}

import State._

//S represents states, A stands for computed value
case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, ss) = run(s)
    (f(a), ss)
  })

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State(s => {
    val (a, s1) = run(s)
    val (b, s2) = sb.run(s1)
    (f(a, b), s2)
  })

  //We could use flatMap to implement map and map2
  def mapViaFlatMap[B](f: A => B): State[S, B] =
    flatMap(a => unit((f(a), _)))

  def map2ViaFlatMap[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap { a: A => sb.map { b: B => f(a, b) } }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, ss) = run(s)
    f(a).run(ss)
  })
}

object State {
  type Rand[A] = State[RNG, A]

  //Exercise 6.10
  def unit[S, A](f: S => (A, S)): State[S, A] = State(f)

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit((Nil, _)): State[S, List[A]])((a: State[S, A], b: State[S, List[A]]) => a.map2(b)(_ :: _))

  //Reading the state
  def get[S]: State[S, S] = State(s => (s, s))

  //Writing the state
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

//Exercise 6.11
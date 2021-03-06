package com.fpinscala.ch02

/**
  * Created by wqlin on 17-10-30 18:48.
  */
object GettingStarted {

  //Exercise 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(i: Int, prev: Int, cur: Int): Int =
      if (i == 0) prev
      else loop(i - 1, cur, prev + cur)

    loop(n, 0, 1)
  }

  //Exercise 2.2
  //For ith element, checking whether ith and i+1th element are sorted
  //By using view, the iteration stops as soon as the ordered predicate return false
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
    (as zip as.tail).view.forall { case (a1, a2) => ordered(a1, a2) }

  //Exercise 2.3
  //Use placeholder syntax.
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => f(a, _)

  //Alternatively, we can implement curry as follows
  //def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  //a => b => f(a, b)


  //Exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = f(_)(_)

  //Alternative implementation
  //def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)


  //Exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

  //Alternative implementation
  //def compose[A, B, C](f: B => C, g: A => B): A => C = f compose g


}

package com.fpinscala.ch03

/**
  * Created by wqlin on 17-10-30 19:51.
  */
sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  //Exercise 3.2
  def tail[A](ls: List[A]): List[A] = ls match {
    case Nil => throw new Error("Nil.tail")
    case Cons(_, xs) => xs
  }

  //Exercise 3.3
  def setHead[A](ls: List[A], newHead: A): List[A] = ls match {
    case Cons(_, xs) => Cons(newHead, xs)
    case _ => throw new Error("Set head on empty list")
  }

  //Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_, t) if n > 0 => drop(t, n - 1)
    case _ => l
  }

  //Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  //Exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new Error("Nil.init")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  //Exercise 3.7
  //No, that is impossible. The reason is that product has to traversing the list all to the end
  //before it applies function f

  //Exercise 3.8
  //By the definition of foldRight, the function call foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_, _)) is expanded as follows:
  //Cons(1, foldRight(List(2, 3), Nil:List[Int])(Cons(_, _))
  //Cons(1, Cons(2, foldRight(List(3), Nil:List[Int])(Cons(_, _))))
  //Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil: List[Int])(Cons(_, _)))))
  //...
  //Cons(1,Cons(2,Cons(3,Nil)))
  //As a result, we get back our original list.

  //Exercise 3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, len) => len + 1)

  //Exercise 3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  //Exercise 3.11
  def sum(as: List[Int]): Int = foldLeft(as, 0)(_ + _)

  def product(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)

  def lengthInFold[A](as: List[A]): Int = foldLeft(as, 1) { case (acc, _) => acc + 1 }

  //Exercise 3.12
  def reverse[A](as: List[A]): List[A] = foldLeft(as, List[A]())((res, ele) => Cons(ele, res))

  //Exercise 3.13
  //Follow implementation in standard List. That is, foldRight can be implemented by combining reverse and foldLeft
  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  /*
  foldLeft and foldRight operation can be illustrate as follows:
  foldLeft:                 foldRight:
         op                        op
         /\                        /\
        . an                      an .
       .                              .
      .                                .
     / \                              / \
    op a2                            a2 op
    /\                                  /\
   z a1                                a1 z
  Implementing foldLeft by using foldRight is very tricky and useless, and one implementation is as follows.
  The following explanation is cited from: https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/datastructures/List.scala
  The implementation builds up a chain of functions which, when called, results in the operations being performed
  with the correct associativity. We are calling `foldRight` with the `B` type being instantiated to `B => B`, then
  calling the built up function with the `z` argument.
  */
  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  //Exercise 3.14
  def appendViaFoldRight[A](xs: List[A], ys: List[A]): List[A] =
    foldRight(xs, ys)(Cons(_, _))

  def appendViaFoldLeft[A](xs: List[A], ys: List[A]): List[A] =
    reverse(foldLeft(ys, reverse(xs))((t, h) => Cons(h, t)))

  //Exercise 3.15
  //Use foldRightViaFoldLeft to avoid stack overflow problem
  def concatAll[A](lss: List[List[A]]): List[A] = foldRightViaFoldLeft(lss, List[A]())(appendViaFoldLeft)

  //Exercise 3.16
  def plusOne(ls: List[Int]): List[Int] = foldRightViaFoldLeft(ls, List[Int]())((h, t) => Cons(h + 1, t))

  //  //Exercise 3.17
  def DoubleToString(ls: List[Double]): List[String] = foldRightViaFoldLeft(ls, List[String]())((h, t) => Cons(h.toString, t))

  //Exercise 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = foldRightViaFoldLeft(as, List[B]())((h, t) => Cons(f(h), t))

  //Alternative implementation
  //def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, List[B]())((h, t) => Cons(f(h), t))

  //Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRightViaFoldLeft(as, List[A]())((h, t) => if (f(h)) Cons(h, t) else t)

  //Exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concatAll(map(as)(f))

  //Alternative implementation
  def flatMapViaFoldRight[A, B](as: List[A])(f: A => List[B]): List[B] = foldRightViaFoldLeft(as, List[B]())((h, t) => appendViaFoldRight(f(h), t))


  //Exercise 3.21
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

  //Exercise 3.22
  def addPair(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPair(t1, t2))
    case _ => Nil
  }

  //Exercise 3.23
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    case _ => Nil
  }

  //Exercise 3.24
  //Note that sub sequence has to be continuous
  @annotation.tailrec
  def startsWith[A](as: List[A], prefix: List[A]): Boolean = (as, prefix) match {
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
    case _ => false
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_, t) => hasSubsequence(t, sub)
  }
}

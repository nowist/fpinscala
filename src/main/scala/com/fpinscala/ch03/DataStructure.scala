package com.fpinscala.ch03

/**
  * Created by wqlin on 17-10-30 19:51.
  */
object DataStructure {
  //Exercise 3.2
  def tail[A](ls: List[A]): List[A] = ls match {
    case Nil => throw new Error("Nil.tail")
    case _ :: xs => xs
  }

  //Exercise 3.3
  def setHead[A](ls: List[A], newHead: A): List[A] = ls match {
    case _ :: xs => newHead :: xs
    case _ => throw new Error("Set head on empty list")
  }

  //Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] =
    if (l.isEmpty || n <= 0) l
    else drop(l.tail, n - 1)

  //Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case h :: t if f(h) => dropWhile(t, f)
    case _ => l
  }

  //Exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new Error("Nil.init")
    case _ :: Nil => Nil
    case x :: xs => x :: init(xs)
  }

  //Exercise 3.9
  def length[A](as: List[A]): Int =
    as.foldRight(0)((_, len) => len + 1)

  //Exercise 3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case h :: t => foldLeft(t, f(z, h))(f)
  }

  //Exercise 3.11
  def sum(as: List[Int]): Int = foldLeft(as, 0)(_ + _)

  def product(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)

  def lengthInFold[A](as: List[A]): Int = foldLeft(as, 1) { case (acc, _) => acc + 1 }

  //Exercise 3.12
  def reverse[A](as: List[A]): List[A] = as.foldLeft(List.empty[A])((res, ele) => ele :: res)

  //Exercise 3.13
  def foldRight[A, B](z: B, as: List[A])(f: (A, B) => B) = foldLeft(reverse(as), z)((a, b) => f(b, a))

  //Exercise 3.14
  def append[A](xs: List[A], ys: List[A]): List[A] =
    xs.foldRight(ys)(_ :: _)

  //Exercise 3.15
  def concatAll[A](lss: List[List[A]]): List[A] = foldRight(List.empty[A], lss)(append)

  //Exercise 3.16
  def plusOne(ls: List[Int]): List[Int] = ls.map(_ + 1)

  //Exercise 3.17
  def DoubleToString(ls: List[Double]): List[String] = ls.map(_.toString)

  //Exercise 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = as.map(f) // could also use ListBuffer instead

  //Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as.foldRight(List.empty[A])((ele, res) => if (f(ele)) ele :: res else res)


  //Exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concatAll(map(as)(f))

  //Exercise 3.21
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

  //Exercise 3.22
  def addPair(as: List[Int], bs: List[Int]): List[Int] = (as zip bs).map { case (a, b) => a + b }

  //Exercise 3.23
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as zip bs).map { case (a, b) => f(a, b) }

  //Exercise 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    val subSet = sub.toSet
    sup.view.foldLeft(List(List[A]())) { case (ls, ele) => ls.map(ele :: _) ::: ls }.exists(_.toSet == subSet)
  }


  // tree definition
  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


  //Exercise 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  //Exercise 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  //Exercise 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  //Exercise 3.28
  def map[A, B](t: Tree[A])(implicit f: A => B): Tree[B] = t match {
    case Leaf(value) => Leaf(f(value))
    case Branch(l, r) => Branch(map(l), map(r))
  }


  //Exercise 3.29
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(value) => f(value)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
}

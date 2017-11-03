package com.fpinscala.ch03

/**
  * Created by wqlin on 17-11-3 09:35.
  */
sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

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

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(v => v)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((l, r) => (l max r) + 1)

  /*
  Note that we have to explicitly state the return type of function. Otherwise, we will get an error:
  type mismatch;
     found   : com.fpinscala.ch03.Branch[B]
     required: com.fpinscala.ch03.Leaf[B]
        fold(t)(value => Leaf(f(value)))(Branch(_, _))
  The reason cited from https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/datastructures/Tree.scala is:
  This error is an unfortunate consequence of Scala using subtyping to encode algebraic data types. Without the
  annotation, the result type of the fold gets inferred as `Leaf[B]` and it is then expected that the second argument
  to `fold` will return `Leaf[B]`, which it doesn't (it returns `Branch[B]`).

   */
  def mapViaFold[A, B, C](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(value => Leaf(f(value)): Tree[B])(Branch(_, _))
}
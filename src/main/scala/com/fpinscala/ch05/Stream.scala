package com.fpinscala.ch05

import scala.collection.immutable.{Stream => _}
import Stream._

/**
  * Created by wqlin on 17-10-31 14:13.
  */
sealed trait Stream[+A] {
  //Exercise 5.1
  def toList: List[A] = {
    var lb = new collection.mutable.ListBuffer[A]

    def toList(s: Stream[A]): List[A] = s match {
      case Empty => lb.toList
      case Cons(h, t) =>
        lb += h()
        toList(t())
    }

    toList(this)
  }

  //Tail recursive solution
  def toListTailRec: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], l: List[A]): List[A] = s match {
      case Empty => l
      case Cons(h, t) => go(t(), h() :: l)
    }

    go(this, Nil).reverse
  }

  //Exercise 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => Empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  //Exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  //Exercise 5.4
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => if (p(h())) t().forAll(p) else false
    case _ => true
  }

  def forAllViaFoldRight(p: A => Boolean): Boolean =
    foldRight(true)((h, t) => p(h) && t)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  //Exercise 5.5
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty)

  //Exercise 5.6
  def headOption: Option[A] =
    foldRight(Option.empty[A])((a, _) => Some(a))

  //Exercise 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  def append[B >: A](ys: => Stream[B]): Stream[B] =
    foldRight(ys)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  //Exercise 5.13
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), i) if i > 1 => Some(h(), (t(), i - 1))
      case (Cons(h, _), i) if i == 1 => Some(h(), (empty, 0))
      case _ => None
    }

  //Bad implementation. Could lead to stack overflow
  //def takeViaUnfold(n: Int): Stream[A] =
  //unfold(this) {
  //case Cons(h, t) if n > 0 => Some(h(), t().takeViaUnfold(n - 1))
  //case _ => None
  //}

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, that)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Empty, Cons(h, t)) => Some((None, Some(h())), (empty, t()))
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), empty))
      case _ => None
    }

  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    zipWith(s2)((_, _))

  //Exercise 5.14
  def startsWith[B >: A](s: Stream[B]): Boolean =
    (this zipAll s).takeWhile(_._2.nonEmpty).forAll {
      case (a, b) => a == b
    }

  //Exercise 5.15
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Cons(h, t) => Some(Cons(h, t), t())
      case _ => None
    } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  //Exercise 5.16
  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] =
    foldRight((z, Stream(z))) { (h, s) =>
      lazy val ss = s
      val hh = f(h, ss._1)
      (hh, cons(hh, ss._2))
    }._2

  //Another implementation. In either case, the method could cause stack overflow error if stream is large
  //    def scanRight[B](z: B)(f: (A, B) => B): Stream[B] = this match {
  //      case Cons(h, t) =>
  //        lazy val s = t().scanRight(z)(f)
  //        s match {
  //          case Cons(hh, _) => cons(f(h(), hh()), s)
  //          case _ => s
  //        }
  //      case _ => Stream(z)
  //    }
}


case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  //Exercise 5.8
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = cons(a, tail)
    tail
  }

  //Exercise 5.9
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  //Exercise 5.10
  def fibs(): Stream[Int] = {
    def fibs(prev: Int, cur: Int): Stream[Int] =
      cons(prev, fibs(cur, prev + cur))

    fibs(0, 1)
  }

  //Exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => Empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }

  //An alternative implementation
  def unfoldViaMap[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map { case (a, s) => cons(a, unfoldViaMap(s)(f)) } getOrElse empty

  //Exercise 5.12
  def onesViaUnfold(): Stream[Int] =
    unfold(1)(_ => Some((1, 1)))

  def fibsViaUnfold(): Stream[Int] =
    unfold((0, 1)) { case (prev, cur) => Some((prev, (cur, prev + cur))) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(x => Some(x, x + 1))

  def constantViaUnfold(n: Int): Stream[Int] =
    unfold(n)(_ => Some(n, n))
}
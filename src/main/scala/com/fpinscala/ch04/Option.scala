package com.fpinscala.ch04

import scala.{Option => _, Either => _, Some => _, None => _}
//hide std library `Option` and `Either

/**
  * Created by wqlin on 17-10-31 10:22.
  */


sealed trait Option[+A] {

  //Exercise 4.1
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(get) => Some(f(get))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(get) => f(get)
  }

  //An alternative implementation for flatMap is using map
  def flatMapViaMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(get) => get
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  //An alternative solution for orElse is using map
  def orElseViaMap[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(get) if f(get) => Some(get)
    case _ => None
  }

  def filterViaFlatMap(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  //Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  //A function that lift ordinary functions to become functions that operate on Option
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  //Exercise 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(a1), Some(b1)) => Some(f(a1, b1))
    case _ => None
  }

  //An alternative implementation
  def map2ViaFlatMap[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  //This implementation demonstrate we can use for-comprehensions to replace flatMap and map
  def map2ViaFor[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  //Exercise 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h.flatMap(hh => sequence(t).map(hh :: _))
  }

  def Try[A](a: => A): Option[A] =
    try Some(a) catch {
      case _: Exception => None
    }

  def sequenceViaFoldRight[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(_ orElse None)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => f(h).flatMap(hh => traverse(t)(f).map(hh :: _))
  }
}

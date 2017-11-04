package com.fpinscala.ch04

import scala.{Option => _, Either => _, Left => _, Right => _}
//hide std library `Option` and `Either
/**
  * Created by wqlin on 17-11-3 22:22.
  */

sealed trait Either[+E, +A] {
  //Exercise 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(value) => Left(value)
    case Right(value) => Right(f(value))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(value) => Left(value)
    case Right(value) => f(value)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(_) => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {a1 <- this
         b1 <- b
    } yield f(a1, b1)
}

case class Left[+E](get: E) extends Either[E, Nothing]

case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  def left[A, E](get: E): Either[E, A] = Left(get)

  def right[A, E](get: A): Either[E, A] = Right(get)

  def Try[A](a: => A): Either[Exception, A] =
    try right(a) catch {
      case e: Exception => left(e)
    }

  //Exercise 4.7
  def traverseViaFoldRight[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(List.empty[B]): Either[E, List[B]]) {
      case (a, e) => (a, e) match {
        case (_, Left(ee)) => Left(ee)
        case (v, Right(ls)) => f(v).map(hh => hh :: ls)
      }
    }

  //Alternative implementation for traverse
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(List.empty[B])
    case h :: t => f(h).flatMap(hh => traverse(t)(f).map(hh :: _))
  }

  //Use map2 to factor out duplication
  def traverseViaMap2[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(Nil): Either[E, List[B]])((a, b) => f(a).map2(b)(_ :: _))

  def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] =
    traverseViaFoldRight(as)(x => x)
}
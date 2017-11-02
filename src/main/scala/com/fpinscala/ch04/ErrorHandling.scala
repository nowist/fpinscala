package com.fpinscala.ch04

import scala.{Option => _, Either => _, _}
//hide std library `Option` and `Either

/**
  * Created by wqlin on 17-10-31 10:22.
  */
object ErrorHandling {

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

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(get) => get
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case _ => this
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(get) if f(get) => Some(get)
      case _ => None
    }
  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  //Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  //Exercise 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(a1), Some(b1)) => Some(f(a1, b1))
    case _ => None
  }

  //Exercise 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???

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

  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]

  //Exercise 4.7
  def traverseEither[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = ???

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = ???


}

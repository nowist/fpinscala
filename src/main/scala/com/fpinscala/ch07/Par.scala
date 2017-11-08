package com.fpinscala.ch07

import java.util.concurrent._

/**
  * Created by wqlin on 17-11-2 15:22.
  */
object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  /*
  unit is represented as a function that returns a UnitFuture. It doesn't use the ExecutorService at all.
   */
  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  /*
  UnitFuture is a simple implementation of Future that just wraps a constant value.
  It's always done and can't be cancelled. Its get method simply returns the value
  that we gave it.
   */
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: A): Par[A] = fork(unit(a))

  //Exercise 7.4
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map2(parList, unit(()))((a, _) => a.sorted)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  //Exercise 7.5
  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(ps.toIndexedSeq))(_.toList)

  //Simple implementation by making use of foldRight
  def sequenceSimple[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(Nil: List[A]))((a, b) => map2(a, b)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  //Exercise 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val fbs: List[Par[List[A]]] = as.map(asyncF(x => if (f(x)) List(x) else List.empty[A]))
    fbs.foldRight(unit(Nil: List[A]))((a, b) => map2(a, b)(_ ::: _))
  }

  //Alternative implementation
  def parFilter_2[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val fbs: List[Par[List[A]]] = as.map(asyncF(x => if (f(x)) List(x) else List.empty[A]))
    map(sequence(fbs))(_.flatten)
  }

  def equal[A](e: ExecutorService)(p1: Par[A], p2: Par[A]): Boolean =
    p1(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  //more general combinator
  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => if (run(es)(cond).get) t(es) else f(es)

  //Exercise 7.11
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
    val i = run(es)(n).get
    choices(i)(es)
  }

  //Exercise 7.12
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = es => {
    val k = run(es)(key).get
    choices(k)(es)
  }

  //Exercise 7.13
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = es => {
    val a = run(es)(pa).get
    choices(a)(es)
  }

  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(if (_) t else f)

  def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(choices(_))

  //Exercise 7.14
  def join[A](a: Par[Par[A]]): Par[A] = es => {
    val pa = run(es)(a).get
    run(es)(pa)
  }

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    join(map(a)(f))


  def choiceViaFlatMap[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    flatMap(cond)(if (_) t else f)

  def choiceNViaFlatMap[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(n)(choices(_))
}
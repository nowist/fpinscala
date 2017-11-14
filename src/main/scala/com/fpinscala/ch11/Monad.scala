package com.fpinscala.ch11

import com.fpinscala.ch06.State
import com.fpinscala.ch07.Par
import com.fpinscala.ch07.Par.Par
import com.fpinscala.ch08.Gen
import com.fpinscala.ch09.Parsers

/**
  * Created by wqlin on 17-11-12 14:33.
  */
//A data type that implements map
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}

//All monads are functors, but not all functors are monads
trait Monad[F[_]] extends Functor[F] {

  //A minimal set of monadic combinators consist of unit and flatMap
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  //Exercise 11.3
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((ls, a) => map2(ls, a)(_ :: _))

  def sequenceViaTraverse[A](lma: List[F[A]]): F[List[A]] =
    traverse(lma)(flatMap(_)(unit(_)))

  def traverseViaSequence[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    sequence(la.map(f(_)))

  //An alternative implementation
  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((a, ls) => map2(f(a), ls)(_ :: _))

  //Exercise 11.4
  //The semantics of `replicateM` is as follows:
  //It replicate the `ma` monadic values `n` times and gathers the results in a single value,
  //where the monad `M` determines how values are actually combined.
  //For `List`, the `replicateM` function will generate a list of lists. It will contain all the
  //lists of length `n` with elements selected from the input list.
  //For `Option`, it will generate either `Some` or `None` based on the input value. If the input
  //is `Some`, then it results in a list that repeats the element n times and is wrapped in Some.
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  //Exercise 11.5

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  //Exercise 11.6
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ms.foldRight(unit(List[A]())) {
    (a, as) => compose(f, (b:Boolean) => if (b) map(as)(a :: _) else as)(a)
  }

  def filterM1[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = map(sequence(ms.map(x => map(f(x))(if (_) List(x) else List[A]()))))(_.flatten)

  //Exercise 11.7
  //A second minimal set of Monad combinators consist of unit and compose
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  //Exercise 11.8
  def flatMapViaCompose[A, B](ma: F[A])(f: A => F[B]): F[B] = compose[F[A], A, B](x => x, f)(ma)

  //Exercise 11.12
  //A third minimal set of Monad combinators consist of unit, map and join
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(fa => fa)

  //Exercise 11.13
  def flatMapViaJoin[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def composeViaJoin[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => join(map(f(a))(g))

  //Exercise 11.14
  /*
  Assume we have:
  val a:A
  val ma: F[A]
  def f: A => F[B]
  def g: B => F[C]
  The monad laws can be restated in terms of join, map and unit
  1. Associativity law:
     join(map(join(map(ma)(f)))(g)) == join(map(ma)(x => join(map(f(x))(g))))
  2. Left identity
     join(map(a)(unit)) == a
  3. Right identity
     join(map(unit(a))(f)) == f(a)
   */
}

object Monad {
  //Implementing Monad for Gen
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)

    def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma flatMap f
  }

  //Exercise 11.1
  val parMonad = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)

    def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(pa)(f)
  }

  def parserMonad[ParseError, P[+ _]](p: Parsers[ParseError, P]) = new Monad[P] {
    def unit[A](a: => A) = p.succeed(a)

    def flatMap[A, B](ma: P[A])(f: A => P[B]) = p.flatMap(ma)(f)
  }

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)

    def flatMap[A, B](oa: Option[A])(f: A => Option[B]): Option[B] = oa flatMap f
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)

    def flatMap[A, B](sa: Stream[A])(f: A => Stream[B]): Stream[B] = sa flatMap f
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)

    def flatMap[A, B](la: List[A])(f: A => List[B]): List[B] = la flatMap f
  }

  val idMonad = new Monad[Id] {
    def unit[A](a: => A): Id[A] = Id(a)

    def flatMap[A, B](ida: Id[A])(f: A => Id[B]): Id[B] = ida.flatMap(f)
  }

  //Exercise 11.2
  //Since `State` is a binary type constructor, we need to partially apply it
  //with the `S` type argument. One solution is to create a class `StateMonads`
  //that accepts the `S` type argument and then has a _type member_ for the
  //fully applied `State[S, A]` type inside:
  class StateMonads[S] {
    type StateS[A] = State[S, A]

    val monad = new Monad[StateS] {
      def unit[A](a: => A): State[S, A] = State(s => (a, s))

      def flatMap[A, B](sa: StateS[A])(f: A => StateS[B]): StateS[B] = sa flatMap f
    }
  }

  //Alternatively, we can create an anonymous class inline, inside parentheses,
  //and project out its type member
  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    def flatMap[A, B](sa: State[S, A])(f: A => State[S, B]): State[S, B] = sa flatMap f
  }

  def getState[S]: State[S, S] = State(s => (s, s))

  def setState[S](s: => S): State[S, Unit] = State(_ => ((), s))

  //Getting and setting state with a for-comprehension
  val F = stateMonad[Int]

  def zipWithIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(F.unit(List[(Int, A)]()))((acc, a) => for {
      xs <- acc
      n <- getState
      _ <- setState(n + 1)
    } yield (n, a) :: xs).run(0)._1.reverse
}

//Exercise 11.17
case class Id[A](value: A) {
  def map[B](f: A => B): B = f(value)

  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

//Exercise 11.20
case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
    def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

    def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader(r => {
      val a = st.run(r)
      f(a).run(r)
    })
  }
}
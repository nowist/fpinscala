package com.fpinscala.ch10

import com.fpinscala.ch07.Par
import com.fpinscala.ch07.Par.Par
import com.fpinscala.ch08.{Gen, Prop}
import com.fpinscala.ch03.{Tree, Leaf, Branch}

/**
  * Created by wqlin on 17-11-10 15:53.
  */
trait Monoid[A] {
  /*
  A monoid is a type together with a binary
   */

  //Satisfies op(op(x, y), z) == op(x, op(y, z))
  def op(a1: A, a2: A): A

  //Satisfies op(x, zero) == x and op(zero, x) == x
  def zero: A
}

object Monoid {
  val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2

    def zero: String = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    val zero: List[A] = Nil
  }

  //Exercise 10.1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2

    def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2

    def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    def zero: Boolean = true
  }

  //Exercise 10.2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    //    def op(a1: Option[A], a2: Option[A]): Option[A] = (a1, a2) match {
    //      case (Some(_), _) => a1
    //      case (None, Some(_)) => a2
    //      case _ => None
    //    }

    def zero: Option[A] = None
  }

  //Exercise 10.3
  //An monoid for an endofunction
  //An endofunction is function which have the same parameter type and return type
  def endoMonoid1[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A): A => A = f compose g

    def zero: A => A = x => x
  }

  def endoMonoid2[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A): A => A = f andThen g

    def zero: A => A = x => x
  }

  //Exercise 10.4
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    Prop.forAll(gen)(a => m.op(a, m.zero) == m.op(m.zero, a) && m.op(a, m.zero) == a) && // Identity law
      Prop.forAll(for {
        x <- gen
        y <- gen
        z <- gen
      } yield (x, y, z)) { case (x, y, z) => m.op(m.op(x, y), z) == m.op(x, m.op(y, z)) } // Associativity law

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  //Exercise 10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldMapViaFoldRight[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldRight(m.zero)((a, b) => m.op(f(a), b))

  //Exercise 10.6
  //Note that foldLeft and foldRight are implemented in different endoMonoid implementation
  def foldLeft[A, B](as: List[A])(z: B)(op: (B, A) => B): B = foldMap(as, endoMonoid2[B])(a => op(_, a))(z)

  def foldRight[A, B](as: List[A])(z: B)(op: (A, B) => B): B = foldMapViaFoldRight(as, endoMonoid1[B])(op.curried)(z)

  //Exercise 10.7
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.isEmpty)
      m.zero
    val len = v.length
    if (len == 1)
      f(v.head)
    else {
      val (l, r) = v.splitAt(len / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }

  //Exercise 10.8
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)

    def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    val pm = par(m)

    def parFoldMap(v: IndexedSeq[A], pm: Monoid[Par[B]])(f: A => B): Par[B] = {
      if (v.isEmpty) pm.zero
      val len = v.length
      if (len == 1)
        Par.lazyUnit(f(v.head))
      else {
        val (l, r) = v.splitAt(v.length / 2)
        pm.op(parFoldMap(l, pm)(f), parFoldMap(r, pm)(f))
      }
    }

    parFoldMap(v, pm)(f)
  }

  //Parallel parsing
  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  //Exercise 10.10
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + w2 + (if ((r1 + l2).isEmpty) 0 else 1), r2)
      case (Part(l, w, r), Stub(s)) => Part(l, w, r + s)
      case (Stub(s), Part(l, w, r)) => Part(s + l, w, r)
      case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
    }

    def zero: WC = Stub("")
  }

  //Exercise 10.11
  def wordCount(s: String): Int = {
    def wc(c: Char): WC =
      if (c.isWhitespace) Part("", 0, "")
      else Stub(c.toString)

    def unstub(s: String): Int = s.length min 1

    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(str) => unstub(str)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }


  //Foldable data structures
  //F[_] is a type constructor
  trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

    def concatenate[A](as: F[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op)

    //Exercise 10.15
    def toList[A](fa: F[A]): List[A] = foldRight(fa)(List.empty[A])(_ :: _)

    def toListViaFoldMap[A](fa: F[A]): List[A] = foldMap(fa)(List(_))(listMonoid)
  }


  //Exercise 10.12
  def ListFoldable: Foldable[List] = new Foldable[List] {
    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))

    override def toList[A](as: List[A]): List[A] = as
  }

  def IndexedSeqFoldable: Foldable[IndexedSeq] = new Foldable[IndexedSeq] {
    def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
      foldMapV(as, mb)(f)
  }

  def StreamFoldable: Foldable[Stream] = new Foldable[Stream] {
    def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B = as match {
      case #::(h, t) => mb.op(f(h), foldMap(t)(f)(mb))
      case _ => mb.zero
    }

  }

  //Exercise 10.13
  val TreeFoldable: Foldable[Tree] = new Foldable[Tree] {
    def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
      case Leaf(a) => f(a, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }

    def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
      case Leaf(a) => f(z, a)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }

    def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
      case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
      case Leaf(v) => f(v)
    }
  }


  //Exercise 10.14
  def optionFoldable: Foldable[Option] = new Foldable[Option] {
    def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
      case Some(a) => f(a)
      case None => mb.zero
    }
  }

  //Exercise 10.16
  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    /*
    By A.op and B.op is associative, we can prove that op is also associative
    Prove:
    we let p1 = (a1, b1), p2 = (a2, b2), p3 = (a3, b3). Then
      op(op(p1, p2), p3)
    = op((a.op(a1, a2), b.op(b1, b2)), (a3, b3))
    = (a.op(a.op(a1, a2), a3), b.op(b.op(b1, b2), b3))
    = (a.op(a1, a.op(a2, a3)), b.op(b1, b.op(b2, b3)))
    = op(p1, (a.op(a2, a3), b.op(b2, b3)))
    = op(p1, op(p2, p3))
     */
    def op(p1: (A, B), p2: (A, B)): (A, B) = (p1, p2) match {
      case ((a1, b1), (a2, b2)) => (A.op(a1, a2), B.op(b1, b2))
    }

    def zero: (A, B) = (A.zero, B.zero)
  }


  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def zero: Map[K, V] = Map[K, V]()

    def op(a: Map[K, V], b: Map[K, V]): Map[K, V] = (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
      acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
    }
  }

  //Exercise 10.17
  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(f: A => B, g: A => B): A => B = a => B.op(f(a), g(a))

    def zero: A => B = _ => B.zero
  }

  //Exercise 10.18
  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))(i => Map(i -> 1))

  //  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
  //    val m: Monoid[Map[A, Int]] = mapMergeMonoid(intAddition)
  //
  //    def bagInner(as: IndexedSeq[A]): Map[A, Int] = {
  //      if (as.isEmpty)
  //        m.zero
  //      val len = as.length
  //      if (len == 1)
  //        Map(as.head -> 1)
  //      else {
  //        val (l, r) = as.splitAt(len / 2)
  //        m.op(bagInner(l), bagInner(r))
  //      }
  //    }
  //
  //    bagInner(as)
  //  }
}

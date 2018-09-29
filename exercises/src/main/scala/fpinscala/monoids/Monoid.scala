package fpinscala.monoids

import fpinscala.monoids.Monoid.{dual, endoMonoid}

import scala.language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2

    val zero = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    val zero: Nil.type = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {

    override def zero: Int = 0

    override def op(a1: Int, a2: Int): Int = a1 + a2
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {

    override def zero: Int = 1

    override def op(a1: Int, a2: Int): Int = a1 * a2
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {

    override def zero: Boolean = false

    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {

    override def zero: Boolean = true

    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {

    override def zero: Option[A] = None

    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {

    override def zero: A => A = a => a

    override def op(a1: A => A, a2: A => A): A => A = a => a2(a1(a))
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  //  import fpinscala.testing._
  //
  //  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
  //    Prop.forAll(gen)(a => m.op(m.zero, a) == a) &&
  //      Prop.forAll(gen)(a => m.op(a, m.zero) == a) &&
  //      Prop.forAll(for {
  //        x <- gen
  //        y <- gen
  //        z <- gen
  //      } yield (x, y, z))(p => m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3))

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    concatenate(as.map(f), m)

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(a => f(a, _))(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid[B])(a => f(_, a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.size >= 2) {
      val splitAs = as.splitAt(as.size / 2)
      m.op(foldMapV(splitAs._1, m)(f), foldMapV(splitAs._2, m)(f))
    } else if (as.size == 1) f(as(0))
    else m.zero

  //  import fpinscala.parallelism._
  //
  //  def par[A](m: Monoid[A]): Monoid[Par[A]] =
  //    new Monoid[Par[A]] {
  //      override def zero: Par[A] = Par.unit(m.zero)
  //
  //      override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)
  //    }
  //
  //  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
  //    foldMapV(v, par(m))(a => Par.map(Par.unit(a))(f))

  val orderedMonoid: Monoid[(Int, Boolean)] = new Monoid[(Int, Boolean)] {

    override def op(a1: (Int, Boolean), a2: (Int, Boolean)): (Int, Boolean) =
      (a2._1, a1._2 && a1._1 < a2._1)

    override def zero: (Int, Boolean) = (Int.MinValue, true)
  }

  def ordered(ints: IndexedSeq[Int]): Boolean =
    foldMapV(ints, orderedMonoid)(a => (a, true))._2

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC


  val wcMonoid: Monoid[WC] = new Monoid[WC] {

    override def zero: WC = Stub("")

    override def op(wc1: WC, wc2: WC): WC = (wc1, wc2) match {

      case (Stub(a), Stub(b)) => Stub(a + b)
      case (Stub(a), Part(lStub, words, rStub)) => Part(a + lStub, words, rStub)
      case (Part(lStub, words, rStub), Stub(b)) => Part(lStub, words, rStub + b)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + w2 + (if ((r1 + l2).isEmpty) 0 else 1), r2)
    }
  }

  def count(s: String): Int = foldMapV(s, wcMonoid)(s => Stub(s.toString)) match {

    case Stub(chars) => if (chars.isEmpty) 0 else 1
    case Part(l, w, r) => w + (if (l.isEmpty) 0 else 1) + (if (r.isEmpty) 0 else 1)
  }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      override def zero: (A, B) = (A.zero, B.zero)

      override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
    }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      override def zero: A => B = _ => B.zero

      override def op(f: A => B, g: A => B): A => B = a => B.op(f(a), g(a))
    }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      override def zero: Map[K, V] = Map[K, V]()

      override def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
        (a.keySet ++ b.keySet).foldLeft(zero)((acc, k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero))))
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val M = mapMergeMonoid[A, Int](intAddition)
    foldMapV(as, M)(a => Map(a -> 1))
  }

}

trait Foldable[F[_]] {

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    Monoid.foldMap(as, mb)(f)
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {

  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    Monoid.foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {

  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => f(_: B, a))(endoMonoid[B])(z)


  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(a => f(a, _: B))(endoMonoid[B])(z)
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case None => mb.zero
    case Some(a) => f(a)
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
    case None => z
    case Some(a) => f(z, a)
  }

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
    case None => z
    case Some(a) => f(a, z)
  }
}
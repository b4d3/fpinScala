package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def toList: List[A] =
    this match {

      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {

    case Empty => empty
    case Cons(h, t) if n >= 1 => cons(h(), t().take(n - 1))
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {

    case Empty => empty
    case Cons(h, t) if n >= 1 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {

    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a))
    cons(a, b) else empty)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = foldRight((b: Option[A]) => b)((a, g) => b => g(b.orElse(Some(a))))(None: Option[A])

  //  Stream(1,2).headOption
  //  (a, g) => b => g(b.orElse(Some(a)))(1, (a, g) => b => g(b.orElse(Some(a)))(2, b => b))(None: Option[A])
  //  b => ((a, g) => b => g(b.orElse(Some(a)))(2, b => b)) (b.orElse(Some(1))) (None)
  //  b => (b => (b => b)(b.orElse(Some(2))) (b.orElse(Some(1))) (None)
  //  (b => (b => b)(b.orElse(Some(2))) (None.orElse(Some(1))
  //  b => (b => b)(b.orElse(Some(2)) Some(1)
  //  (b => b) Some(1).orElse(Some(2))
  //  (b => b) Some(1)
  //  Some(1)

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](streamToAppend: => Stream[B]): Stream[B] = foldRight(streamToAppend)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => f(a).append(b))

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Empty => None
    case Cons(h, t) => Some(f(h()), t())
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((n, this)) {
    case (m, Cons(h, t)) if m >= 1 => Some(h(), (m - 1, t()))
    case _ => None
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {

    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {

    case (_, Empty) => None
    case (Empty, _) => None
    case (Cons(h, t), Cons(h2, t2)) => Some(f(h(), h2()), (t(), t2()))
  }

  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {

    case (Cons(h, t), Empty) => Some(((Some(h()), None), (t(), Empty)))
    case (Empty, Cons(h, t)) => Some(((None, Some(h())), (Empty, t())))
    case (Cons(h, t), Cons(h2, t2)) => Some(((Some(h()), Some(h2())), (t(), t2())))
    case _ => None
  }

  def startsWith[B](s: Stream[B]): Boolean = if (this.zipAll(s).exists {
    case (None, _) => true
    case _ => false
  }) false else
    this.zipWith(s)(
      (a, b) => a == b).forAll(p => p)


  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case s => Some(s, s.drop(1))
  } append Stream(empty)


  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = foldRight(Stream(z)) { (a, b) =>

    b match {
      case Cons(h, t) => cons(f(a, h()), b)
    }
  }

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
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs(n1: Int, n2: Int): Stream[Int] = cons(n1, fibs(n2, n1 + n2))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {

    case None => empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  def onesViaUnfold: Stream[Int] = unfold(0)(s => Some(1, s))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(0)(s => Some(a, s))

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

  def fibsViaUnfold(n1: Int, n2: Int): Stream[Int] = unfold((n1, n2))(s => Some(s._1, (s._2, s._1 + s._2)))

  def main(args: Array[String]): Unit = {

    println(Stream(1, 2, 3).toList)

    assert(Stream(1, 2, 3, 4).take(2).toList == List(1, 2))
    assert(Stream(1, 2, 3, 4).drop(2).toList == List(3, 4))
    assert(Stream(1, 2, 3, 4).drop(0).toList == List(1, 2, 3, 4))
    assert(Stream(1, 2, 3, 4).drop(5).toList == List())

    assert(Stream(6, 2, 3).takeWhile(_ % 2 == 0).toList == List(6, 2))
    assert(Stream(1, 2, 3).takeWhile(_ % 2 == 3) == Empty)
    assert(Stream(1, 2, 3).takeWhile(_ < 10).toList == List(1, 2, 3))

    assert(Stream(6, 2, 3).takeWhile(_ % 2 == 0).toList == Stream(6, 2, 3).takeWhileViaFoldRight(_ % 2 == 0)
      .toList)
    assert(Stream(1, 2, 3).takeWhile(_ % 2 == 3) == Stream(1, 2, 3).takeWhileViaFoldRight(_ % 2 == 3))
    assert(Stream(1, 2, 3).takeWhile(_ < 10).toList == Stream(1, 2, 3).takeWhileViaFoldRight(_ < 10).toList)

    assert(Stream(1, 2, 3).headOption == Some(1))
    assert(Stream().headOption == None)

    assert(Stream(1, 2, 3).map(_ * 2).toList == List(2, 4, 6))
    assert(Stream(1, 2, 3).filter(_ % 2 == 0).toList == List(2))
    assert(Stream(1, 2, 3).append(Stream(4, 5, 6)).toList == List(1, 2, 3, 4, 5, 6))
    assert(Stream(1, 2, 3).flatMap(i => Stream(i, i)).toList == List(1, 1, 2, 2, 3, 3))
    assert(Stream(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList == List(12, 14))

    assert(fibs(0, 1).take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
    assert(onesViaUnfold.take(3).toList == List(1, 1, 1))
    assert(constantViaUnfold(5).take(3).toList == List(5, 5, 5))
    assert(fromViaUnfold(5).take(3).toList == List(5, 6, 7))
    assert(fibsViaUnfold(0, 1).take(7).toList == List(0, 1, 1, 2, 3, 5, 8))

    assert(Stream(1, 2, 3).mapViaUnfold(_ * 2).toList == List(2, 4, 6))
    assert(Stream(1, 2, 3, 4).takeViaUnfold(2).toList == List(1, 2))
    assert(Stream(6, 2, 3).takeWhileViaUnfold(_ % 2 == 0).toList == List(6, 2))
    assert(Stream(1, 2, 3).takeWhileViaUnfold(_ % 2 == 3) == Empty)
    assert(Stream(1, 2, 3).takeWhileViaUnfold(_ < 10).toList == List(1, 2, 3))

    assert(Stream(1, 2, 3).zipWith(Stream(4, 5, 6))(_ + _).toList == List(5, 7, 9))
    assert(Stream(1, 2, 3).zipAll(Stream(5, 7)).toList == List((Some(1), Some(5)), (Some(2), Some(7)),
      (Some(3), None)))

    assert(empty.startsWith(empty))
    assert(Stream(1, 2).startsWith(empty))
    assert(!empty.startsWith(Stream(1, 2)))
    assert(Stream(1, 2, 3).startsWith(Stream(1, 2)))
    assert(Stream(1, 2, 3).startsWith(Stream(1, 2, 3)))
    assert(!Stream(1, 2).startsWith(Stream(1, 2, 3)))
    assert(!Stream(1, 3, 2).startsWith(Stream(1, 2, 3)))

    assert(Stream(1, 2).tails.mapViaUnfold(_.toList).toList == List(List(1, 2), List(2), List()))

    println(Stream(1, 2, 3).scanRight(0)(_ + _).toList)
  }
}
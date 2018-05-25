package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.

  def fill[A](n: Int)(a: A): List[A] = n match {
    case 0 => Nil
    case _ => Cons(a, fill(n - 1)(a))
  }

  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // This pattern is matched
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new UnsupportedOperationException("Nil does not have a tail")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case a if a <= 0 => l
    case _ => drop(tail(l), n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, b) => b + 1)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {

    @tailrec
    def loop(currentList: List[A], acc: B): B = currentList match {
      case Nil => acc
      case Cons(a, as) => loop(as, f(acc, a))
    }

    loop(l, z)
  }

  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Int]): Int = foldLeft(ns, 1)(_ * _)

  def length3[A](as: List[A]): Int = foldLeft(as, 0)((b, _) => b + 1)

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((a, b) => Cons(b, a))

  def foldRightViaLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b,
                                                                                               a) => f(a, b))

  def foldLeftViaRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(as, z)((b,
                                                                                       a) => f(a, b))

  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((a, l) => Cons(a, l))

  def concat[A](listOfAs: List[List[A]]): List[A] = listOfAs match {

    case Nil => Nil
    case Cons(h, t) => foldLeft(t, h)((flattedList, l) =>
      append(flattedList, l))
  }

  def add1(ns: List[Int]): List[Int] = foldRight(ns, Nil: List[Int])((a, b) => Cons(a + 1, b))

  def doublesToStrings(ns: List[Double]): List[String] = foldRight(ns, Nil: List[String])((a, b) =>
    Cons(a.toString, b))

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((a, b) => Cons(f(a), b))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])((a, b) => if
  (f(a)) Cons(a, b) else b)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((a, bs) => append(f(a), bs))

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def pairwiseAdder(ns1: List[Int], ns2: List[Int]): List[Int] =
  // See zipWith for more concise matching
    ns1 match {
      case Nil => Nil
      case Cons(h1, t1) => {
        ns2 match {
          case Nil => Nil
          case Cons(h2, t2) => Cons(h1 + h2, pairwiseAdder(t1, t2))
        }
      }
    }

  def zipWith[A](as1: List[A], as2: List[A])(f: (A, A) => A): List[A] = (as1, as2) match {

    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def take[A](as: List[A], n: Int): List[A] = {

    def loop(original: List[A], size: Int): List[A] =
      if (n == size) Nil
      else original match {
        case Nil => throw new IndexOutOfBoundsException
        case Cons(h, t) => Cons(h, loop(t, size + 1))
      }

    loop(as, 0)
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

    val supLength = length(sup)
    val subLength = length(sub)

    @tailrec
    def loop(currentSup: List[A], currentSupLength: Int): Boolean =
      if (currentSupLength < subLength) false
      else currentSup match {
        case Nil => true  // We know the both lists are Nil because of if condition above
        case Cons(h, t) =>
          if (take(currentSup, subLength) == sub) true
          else loop(t, currentSupLength - 1)
      }

    loop(sup, supLength)
  }


  def main(args: Array[String]): Unit = {

    println(init(List(1, 2, 3, 4)))
    println(foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))
    println(length(List(1, 2, 3, 4, 5)))

    println(foldLeft(List(1, 2, 3, 4, 5, 1, 2, 3), 0)(_ + _))

    println(sum3(List(1, 3, 4, 5)))
    println(product3(List(1, 3, 4, 5)))
    println(length3(List(1, 3, 4, 5)))

    println(reverse(List(1, 2, 3)))

    println(foldRightViaLeft(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))

    println(foldRight(List(1, 2, 3), 0)(_ - _))
    println(foldRightViaLeft(List(1, 2, 3), 0)(_ - _))

    println(foldLeft(List(1, 2, 3, 4), 0)(_ - _))
    println(foldLeftViaRight(List(1, 2, 3, 4), 0)(_ - _))

    println(appendViaFoldRight(List(1, 2, 3), List(4, 5, 6)))

    println(concat(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))))

    println(add1(List(1, 2, 3)))

    val strs: List[String] = doublesToStrings(List(1d, 2d, 3d))
    println(strs)

    println(map(List(1, 2, 3))(_ * 2))

    println(filter(List(1, 2, 3))(_ % 2 == 0))

    println(flatMap(List(1, 2, 3))(i => List(i, i)))

    println(filterViaFlatMap(List(1, 2, 3))(_ % 2 == 0))

    println(pairwiseAdder(List(1, 2, 3), List(4, 5, 6)))

    println(zipWith(List(1, 2, 3), List(4, 5, 6))(_ * _))
    println(hasSubsequence(Nil, List(1,2,3,4)))
  }
}

package fpinscala.errorhandling

import scala.util.{Try => _}
import scala.{Either => _, Option => _, Some => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {

    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {

    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {

    case None => None
    case Some(a) => f(a)
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {

    case None => ob
    case Some(_) => this
  }

  def filter(f: A => Boolean): Option[A] = this match {

    case Some(a) if f(a) => Some(a)
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]


object Option {

  def failingFn(i: Int): Int = {

    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch {
      case e: Exception => 43
    } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (None, _) => None
    case (_, None) => None
    case (Some(sa), Some(sb)) => Some(f(sa, sb))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = if (a.contains(None)) None else Some {
    a.map {
      case Some(sa) => sa
    }
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => f(h) flatMap (b => traverse(t)(f).map(l => b :: l))
  }

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(a => a)

  def main(args: Array[String]): Unit = {

    assert(variance(List(600, 470, 170, 430, 300)) == Some(21704.0))

    assert(sequence(List(Some(2), None, Some(5))) == None)
    assert(sequence(List(Some(2), Some(8), Some(5))) == Some(List(2, 8, 5)))

    val strings = List("3", "a", "5")
    val intStrings = List("3", "7", "5")


    def Try[A](a: => A): Option[A] = {
      try Some(a)
      catch {
        case e: Exception => None
      }
    }

    assert(traverse(strings)(s => Try(s.toInt)) == None)
    assert(traverse(intStrings)(s => Try(s.toInt)) == Some(List(3, 7, 5)))

    assert(sequence(List(Some(2), None, Some(5))) == sequenceViaTraverse(List(Some(2), None, Some(5))))
    assert(sequence(List(Some(2), Some(8), Some(5))) == sequenceViaTraverse(List(Some(2), Some(8), Some(5))))
  }
}
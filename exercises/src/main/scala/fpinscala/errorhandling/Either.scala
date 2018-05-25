package fpinscala.errorhandling


import fpinscala.errorhandling.Option.traverse

import scala.{Either => _, Left => _, Option => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {

    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {

    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {

    case Left(_) => b
    case Right(_) => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {

    case (Left(e), _) => Left(e)
    case (_, Left(e)) => Left(e)
    case (Right(a), Right(b)) => Right(f(a, b))
  }

}

case class Left[+E](get: E) extends Either[E, Nothing]

case class Right[+A](get: A) extends Either[Nothing, A]

object Either {

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {

    case Nil => Right(Nil)
    case h :: t => f(h) flatMap (b => traverse(t)(f).map(l => b :: l))
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(a => a)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }


  def main(args: Array[String]): Unit = {


    assert(sequence(List(Right(2), Left("nono"), Right(5))) == Left("nono"))
    assert(sequence(List(Right(2), Right(8), Right(5))) == Right(List(2, 8, 5)))

    val strings = List("3", "a", "5")
    val intStrings = List("3", "7", "5")

    assert(traverse(strings)(s => Try(s.toInt)).isInstanceOf[Left[NumberFormatException]])
    assert(traverse(intStrings)(s => Try(s.toInt)) == Right(List(3, 7, 5)))

  }
}
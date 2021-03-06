package fpinscala.parsing

import fpinscala.testing._

import scala.language.higherKinds
import scala.util.matching.Regex

trait Parsers[Parser[+_]] {
  self => // so inner classes may call methods of trait

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  type Parser[+A] = Location => Result[A]

  trait Result[+A] {

    def mapError(f: ParseError => ParseError): Result[A] = this match {

      case Failure(e, isCommitted) => Failure(f(e), isCommitted)
      case _ => this
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, isCommitted = false)
      case _ => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e, c) => Failure(e, c || isCommitted)
      case _ => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, m) => Success(a, m + n)
      case _ => this
    }
  }

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]

  case class Failure(get: ParseError, isCommitted: Boolean = true) extends Result[Nothing]

  def attempt[A](p: Parser[A]): Parser[A] = s => p(s).uncommit

  def label[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.label(msg))

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def succeed[A](a: A): Parser[A] = _ => Success(a, 0)

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  implicit def string(s: String): Parser[String] = (location: Location) => {

    if (location.input.startsWith(s))
      Success(s, s.length)
    else Failure(location.toError("").label(s"Expected: $s"), isCommitted = false)
  }

  def or[A](x: Parser[A], y: => Parser[A]): Parser[A] = s => x(s) match {
    case Failure(_, false) => y(s)
    case r => r
  }

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = s => p(s) match {
    case Success(a, n) => f(a)(s.advanceBy(n)).addCommit(n != 0).advanceSuccess(n)
    case e@Failure(_, _) => e
  }

  implicit def regex(r: Regex): Parser[String] = location => location.input match {

    case r(s) => Success(s, s.length)
    case _ => Failure(location.toError(s"Expected string of patter: ${r.toString()}"), isCommitted = false)
  }

  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = p(Location(input)) match {
    case Success(a, _) => Right(a)
    case Failure(e, _) => Left(e)
  }

  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) | succeed(List())

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = p.flatMap(a => succeed(f(a)))

  def slice[A](p: Parser[A]): Parser[String] = location => p(location) match {

    case Success(_, charsConsumed) => Success(location.input.slice(0, charsConsumed), charsConsumed)
    case Failure(parseError, isCommitted) => Failure(parseError, isCommitted)
  }

  def many1[A](p: Parser[A]): Parser[List[A]] = map(p ** many(p))(a => a._1 :: a._2)

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = productViaFlatMap(p, p2)

  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    (p1 ** p2).map(f.tupled)

  def contextSensitive[A](n: Parser[Int], p: Parser[A]): Parser[List[A]] = "\\d+".r.flatMap(i => listOfN(i.toInt, p))

  def productViaFlatMap[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    p.flatMap(a => p2.map((a, _)))

  def map2ViaFlatMap[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    p1.flatMap(a => p2.map(f(a, _)))

  def skipLeft[B](pa: Parser[Any], pb: Parser[B]): Parser[B] = map2(pa, pb)((_, b) => b)

  def skipRight[A](pa: Parser[A], pb: Parser[Any]): Parser[A] = map2(pa, pb)((a, _) => a)

  def whitespace: Parser[String] = regex("\\s+".r)

  def skipWhitespaces[A](p: Parser[A]): Parser[A] = p <* whitespace

  def doubleString: Parser[String] = regex("(-\\d)?\\d*)\\.?\\d+([eE](+|-)?\\d+)?".r)

  def double: Parser[Double] = doubleString map (_.toDouble)

  def nil: Parser[String] = string("null")

  def bool: Parser[Boolean] = (string("true") or string("false")) map (_.toBoolean)

  def surround[A](pc: Parser[Char], pa: Parser[A], pc2: Parser[Char]): Parser[A] =
    (pc ** pa).map(p => p._2) ** pc2 map (p => p._1)

  def extractSeparatedValues[A](separator: Parser[Char], p: Parser[A]): Parser[List[A]] =
    (many(p.skipWhitespaces ** separator) map (_.map(_._1))) ** p map (p => p._2 :: p._1)

  case class ParserOps[A](p: Parser[A]) {

    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = self.many(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def slice: Parser[String] = self.slice(p)

    def numA(c: Char): Parser[Int] = char(c).many.slice.map(_.length)

    def many1: Parser[List[A]] = self.many1(p)

    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def skipWhitespaces: Parser[A] = self.skipWhitespaces(p)

    def *>[B](pb: Parser[B]): Parser[B] = self.skipLeft(p, pb)

    def <*(pb: Parser[Any]): Parser[A] = self.skipRight(p, pb)

  }

  object Laws {

    //    a | b == b | a
    //    a | (b | c) == (a | b) | c
    //    a & b == b & a
    //    a (b & c) == (a & b) & c
    //    a | 0 == a
    //    a & 0 == 0

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)

    def productLaw[A, B, C](p1: Parser[A], p2: Parser[B], p3: Parser[C])(in: Gen[String]): Prop =
      equal(p1 ** (p2 ** p3), (p1 ** p2) ** p3)(in)

    def productLaw2[A, B, C](p1: Parser[A], p2: Parser[B], p3: Parser[C])(in: Gen[String]): Prop =
      equal(p1 ** (p2 | p3), (p1 ** p2) | (p1 ** p3))(in)
  }

}

case class Location(input: String, offset: Int = 0) {

  lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col: Int = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int): Location = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""
}

case class ParseError(stack: List[(Location, String)] = List(),
                      otherFailures: List[ParseError] = List()) {

  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)

  def label[A](s: String): ParseError =
    ParseError(latestLoc.map((_, s)).toList)

  def latest: Option[(Location, String)] =
    stack.lastOption

  def latestLoc: Option[Location] =
    latest map (_._1)
}
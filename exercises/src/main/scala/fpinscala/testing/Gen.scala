package fpinscala.testing

import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.laziness.Stream
import fpinscala.parallelism.Par
import fpinscala.parallelism.Par.Par
import fpinscala.state._
import fpinscala.testing.Gen.{choose, unit, weighted}
import fpinscala.testing.Prop.{MaxSize, _}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

// Exercise 8.1
// forAll(intList)(ns => ns.sum == ns.reverse.sum)
// forAll(intList)(ns => ns.filter(_ == ns.head).sum == ns.filter(_ == ns.head).size * ns.head)
// forAll(intList)(ns => {
//    val (l, r) = ns.splitAt(ns.length / 2)
//    l.sum + r.sum == ns.sum
// }

// Exercise 8.2
// forAll(intList)(ns => ns.max == ns.sorted.reverse.head)


case class Gen[+A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(this.sample.flatMap(f(_).sample))

  def map[B](f: A => B): Gen[B] = Gen(this.sample.map(f(_)))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def listOfNViaFlatmap(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen.listOfN(n, this))

  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {

  val boolean: Gen[Boolean] =
    Gen(State(RNG.map(RNG.nonNegativeLessThan(2))(n => if (n == 1) true else false)))

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(if (_) g1 else g2)


  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(RNG.map(RNG
    .nonNegativeLessThan(stopExclusive - start))(_ + start)))

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = Gen(State(rng => {
    val (randD, rng2) = RNG.double(rng)
    val weight1 = g1._2
    val weight2 = g2._2

    val probability1 = weight1 / (weight1 + weight2)
    val probability2 = weight2 / (weight1 + weight2)

    {
      if (probability1 < probability2) {
        if (probability1 >= randD)
          g1._1.sample
        else g2._1.sample
      } else if (probability2 >= randD)
        g2._1.sample
      else g1._1.sample
    }.run(rng2)
  }))


  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => listOfN(n, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => {
    if (n < 1) listOfN(1, g)
    else listOfN(n, g)
  })

  val maxProp: Prop = forAll(listOf1(Gen.choose(-10, 10))) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  val sortedProp1: Prop = forAll(listOf(Gen.choose(-10, 10))) { ns =>
    ns.sorted.sorted == ns
  }

  val sortedProp2: Prop = forAll(listOf(Gen.choose(-10, 10))) { ns =>
    ns.isEmpty || ns.size == 1 || ns.sorted.takeRight(ns.size - 1).zip(ns.sorted.drop(1)).forall(pair => pair._1 < pair._2)
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    val list = List.fill(n)(g.sample)
    Gen(State.sequence(list))
  }
}

sealed trait Result {
  def isFalsified: Boolean
}

case object Proved extends Result {
  override def isFalsified: Boolean = false
}

case object Passed extends Result {
  override def isFalsified: Boolean = false
}

case class Falsified(failure: FailedCase, successCount: SuccessCount) extends Result {
  override def isFalsified: Boolean = true
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {

        case Proved | Passed => p.run(max, n, rng) match {
          case Proved | Passed => Passed
          case Falsified(msg, successCount) => Falsified(s"RIGHT: $msg", successCount)
        }
        case Falsified(msg, successCount) => Falsified(s"LEFT: $msg", successCount)
      }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Proved | Passed => Passed
        case Falsified(msg, _) => p.run(max, n, rng) match {
          case Proved | Passed => Passed
          case Falsified(msg2, successCount) => Falsified(msg + "\n" + msg2, successCount)
        }
      }
  }
}

object Prop {

  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  type MaxSize = Int

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  // Specialized forAll that Proves the code automatically
  def forAll(gen: Gen[Boolean])(f: Boolean => Boolean): Prop = Prop {
    (n, max, rng) => {
      val trueResult = try {
        if (f(true)) Proved
        else Falsified("true", 0)
      } catch {
        case e: Exception => Falsified("true", 0)
      }

      val falseResult = try {
        if (f(false)) Proved
        else Falsified("false", 1)
      } catch {
        case e: Exception => Falsified("false", 1)
      }

      (trueResult, falseResult) match {
        case (_: Falsified, _) => trueResult
        case (_, _: Falsified) => falseResult
        case _ => Proved
      }
    }
  }

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, max, rng) =>
      randomStream(gen)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  val ES: ExecutorService = Executors.newCachedThreadPool

  val p2: Prop = check {
    val p = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    p(ES).get == p2(ES).get
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)

  val p3: Prop = check {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )(ES) get
  }

  val S: Gen[ExecutorService] = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25) // `a -> b` is syntax sugar for `(a,b)`

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S.map2(g)((_, _))) { case (s, a) => f(a)(s).get }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  val forkProp: Prop = checkPar(equal(Par.fork(Par.unit(1)), Par.unit(1)))

  def genStringIntFn(g: Gen[Int]): Gen[String => Int] = g map (i => s => s.hashCode % i)

  val isEven: Int => Boolean = (i: Int) => i % 2 == 0

  def filterProp: Prop = Prop.forAll(Gen.listOf(Gen(State.int)))(ns => ns.filter(isEven).forall(isEven))

  def takeDropProp: Prop = Prop.forAll(Gen.listOf(Gen(State.int)))(ns => ns.takeRight(ns.length - 1) ==
    ns.drop(1))
}

case class SGen[+A](forSize: Int => Gen[A]) {

  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] = SGen(n => forSize(n).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(n => forSize(n).flatMap(a => f(a).forSize(n)))

}


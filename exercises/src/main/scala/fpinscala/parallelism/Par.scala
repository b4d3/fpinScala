package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](a: A) extends Future[A] {
    def isDone = true

    def get(): A = get(10, TimeUnit.SECONDS)

    def get(timeout: Long, units: TimeUnit): A = a

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C, timeout: Long = Long.MaxValue,
                                          units: TimeUnit = TimeUnit.DAYS): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)

      val startTime = TimeUnit.NANOSECONDS.convert(System.nanoTime(), units)

      val valueA = af.get(timeout, units)

      val diffTime = TimeUnit.NANOSECONDS.convert(System.nanoTime(), units) - startTime

      val valueB = bf.get(diffTime, units)

      UnitFuture(f(valueA, valueB))
    }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    es => UnitFuture(ps.foldRight(Nil: List[A])((a, b) => a(es).get :: b))

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = fork {

    val fbs: List[Par[B]] = as.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A, B](as: List[A])(f: A => Boolean): Par[List[A]] = fork {

    val parFilters: List[Par[(A, Boolean)]] = as.map(asyncF(a => (a, f(a))))

    parFilters.foldRight[Par[List[A]]](unit(List()))((parA, parB) => map2(parA, parB)((a, b) =>
      if (a._2) a._1 :: b
      else b))
  }

  def fold[A](z: A)(as: IndexedSeq[A])(f: (A, A) => A): Par[A] = {

    if (as.length <= 1)
      Par.unit(as.headOption getOrElse z)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      Par.map2(Par.fork(fold(z)(l)(f)), Par.fork(fold(z)(r)(f)))(f)
    }
  }

  def max(as: IndexedSeq[Int]): Par[Int] = fold(Int.MinValue)(as)((l, r) => if (l < r) l else r)

  def countWords(paragraphs: List[String]): Par[Int] =
    paragraphs.map(asyncF(_.split("\\s+").length)).foldRight(unit(0))(map2(_, _)(_ + _))

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] =
    map2(map2(a, b)((a, b) => f(a, b, _)), c)((ab, c) => ab(c))

  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] =
    map2(map3(a, b, c)((a, b, c) => f(a, b, c, _)), d)((abc, d) => abc(d))

  def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C,
    D, E) => F): Par[F] =
    map2(map4(a, b, c, d)((a, b, c, d) => f(a, b, c, d, _)), e)((abcd, e) => abcd(e))


  // map(x)(f) = f(x)
  // map(map(y)(g))(f) = f(map(y)(g))
  // map(map(y)(g))(f) = f(g(y))
  // map(map(y)(g))(f) = (f compose g)(y)
  // map(map(y)(g))(f) = map(y)(f compose g)


  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }

}

object Examples {

  import Par._

  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

  def main(args: Array[String]): Unit = {

    println(fold(0)(Vector(1, 2, 3, 4))(_ + _)(Executors.newFixedThreadPool(4)).get)
  }
}

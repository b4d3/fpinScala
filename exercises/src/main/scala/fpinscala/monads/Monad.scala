package fpinscala
package monads

import fpinscala.parallelism.Nonblocking.Par
import fpinscala.state.State
import fpinscala.testing._

import scala.language.higherKinds


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
  val listFunctor: Functor[List] = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight(unit(Nil: List[A]))((a, b) => map2(a, b)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] = sequence(la.map(f))

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = sequence(List.fill(n)(ma))

  def product[A, B](ma: M[A], mb: M[B]): M[(A, B)] = map2(ma, mb)((_, _))

  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
    ms.foldRight(unit(Nil: List[A]))((a, mb) => map2(f(a), mb)((cond, b) => if (cond) a :: b else
      b))

  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] = a => flatMap(f(a))(g)

  // Implement in terms of `compose`:
  def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = compose((_: Unit) => ma, f)(())

  // x.flatMap(f).flatMap(g) == compose(f, g)(x)
  // Some(v).flatMap(f).flatMap(g) == (a => flatMap(f(a))(g))(v)
  // f(v).flatMap(g) == flatMap(f(v))(g)
  // f(v).flatMap(g) == f(v).flatMap(g)

  // compose(f, unit)(a) == flatMap(x)(unit)
  // (a => flatMap(f(a))(unit))(a) == flatMap(x)(unit)
  // flatMap(x)(unit) == flatMap(x)(unit)

  // compose(f, unit) == compose(unit, f)
  // a => flatMap(f(a))(unit) == a => flatMap(unit(a))(f)
  // flatMap(Some(b))(unit) == flatMap(Some(a))(f)
  // Some(b).flatMap(a => a) == Some(a).flatMap(a => f(a))
  // Some(b) == Some(b)

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(ma => ma)

  // Implement in terms of `join`:
  def __flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad: Monad[Gen] = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  //  def parserMonad[P[+ _]](p: Parsers[P]): Monad[P] = new Monad[P] {
  //    override def flatMap[A, B](ma: P[A])(f: A => P[B]): P[B] = p.flatMap(ma)(f)
  //
  //    override def unit[A](a: => A): P[A] = p.succeed(a)
  //  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma flatMap f

    override def unit[A](a: => A): Option[A] = Some(a)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma flatMap f

    override def unit[A](a: => A): Stream[A] = Stream(a)
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma flatMap f

    override def unit[A](a: => A): List[A] = List(a)
  }

  class _StateMonads[S] {

    type StateS[A] = State[S, A]

    val stateMonadA: Monad[StateS] = new Monad[StateS] {
      override def unit[A](a: => A): StateS[A] = State(s => (a, s))

      override def flatMap[A, B](ma: StateS[A])(f: A => StateS[B]): StateS[B] = ma flatMap f
    }
  }

  def stateMonad[S]: Monad[State[S, _]] = new Monad[({type f[X] = State[S, X]})#f] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] = ma flatMap f
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)

    override def unit[A](a: => A): Id[A] = Id(a)
  }

}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))

  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Reader {

  def readerMonad[R]: Monad[Reader[R, _]] = new Monad[({type f[x] = Reader[R, x]})#f] {
    def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

    override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      Reader(r => {
        val a = st.run(r)
        f(a).run(r)
      })
  }
}


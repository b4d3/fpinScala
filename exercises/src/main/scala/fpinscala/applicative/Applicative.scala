package fpinscala
package applicative

import fpinscala.applicative.StateUtil._
import fpinscala.monads.Functor
import fpinscala.monoids._
import fpinscala.state._

import scala.language.{higherKinds, implicitConversions}

trait Applicative[F[_]] extends Functor[F] {

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(a => f(a, _: B)))(fb)

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(map(fa)(f.curried))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(map(fa)(f.curried))(fb))(fc))(fd)

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((f, a) => f(a))

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(a => a)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {

    val self = this

    new Applicative[({type f[x] = (F[x], G[x])})#f] {

      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

      override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
        (self.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))
    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {

    val self = this

    new Applicative[({type f[x] = F[G[x]]})#f] {

      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fga, fgb)((ga, gb) => G.map2(ga, gb)((a, b) => f(a, b)))
    }
  }

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldRight(unit(Map[K, V]()))((kFv, fb) => map2(kFv._2, fb)((v, b) => b + (kFv._1 -> v)))
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))

  // Not possible for Monads
  //  def compose[G[_]](g: Monad[G]): Monad[({ type f[x] = F[G[x]]})#f] = {
  //
  //    val self = this
  //
  //    new Monad[({type f[x] = F[G[x]]})#f] {
  //
  //      override def flatMap[A, B](ma: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
  //        self.flatMap(ga => g.flatMap(ga)(a => f(a)))
  //
  //      override def unit[A](a: => A): F[G[A]] = self.unit(g.unit(a))
  //    }
  //  }

  // ======== Proofs that Monad implementations satisfy applicative laws ========

  // == IDENTITY LAW ==
  //  map(v)(id) == v
  //  flatMap(v)(a => unit((x => x)(a))) == v
  //  flatMap(v)(a => unit(a)) == v

  //  map(map(v)(g))(a => unit(f(a))) == map(v)(f compose g)
  //  flatMap( flatMap(v)(a => unit(g(a))) )(a => unit(f(a))) == flatMap(v)(a => unit(f(g(a))))
  //  flatMap( unit(g(v)) )(a => unit(f(a))) == flatMap(v)(a => unit(f(g(a))))
  //  (a => unit(f(a)))(g(v)) == unit(f(g(v)))
  //  unit(f(g(v))) == unit(f(g(v)))

  // == LEFT IDENTITY ==
  //  map2(unit(()), fa)((_, a) => a) == fa
  //  flatMap(unit(()))(a => map(fa)(b => ((_, a) => a)(a, b))) == fa
  //  map(fa)(b => ((_, a) => a)((), b)) == fa
  //  map(fa)(b => b) == fa
  //  fa

  // == ASSOCIATIVITY ==
  //  product(product(fa, fb), fc) == map(product(fa, product(fb, fc)))(assoc)
  //  map2(map2(fa, fb)((_, _)), fc)((_, _)) == map(product(fa, product(fb, fc)))(assoc)
  //  flatMap(flatMap(fa)(a => map(fb)(b => (a, b))))(ab => map(c)(c => (ab, c))) == map(product(fa, product(fb, fc)))(assoc)
  //  flatMap((a, b))(ab => map(c)(c => (ab, c))) == map(flatMap(fa)(a => map((b, c))(bc => (a, bc))))(assoc)
  //  ((a, b), c) == map((a, (b, c)))(assoc)
  //  ((a, b), c) == ((a, b), c)

  //  == NATURALITY OF PRODUCT ==
  //  map2(a, b)(productF(f, g)) == product(map(a)(f), map(b)(g))
  //  map2(a, b)((i, i2) => (f(i), g(i2)) (f, g)) == product(map(a)(f), map(b)(g))
  //  flatMap(a)(a => map(b)(b => ((i, i2) => (f(i), g(i2))) (a, b)) == map2(map(a)(f), map(b)(g))((_, _))
  //  (f(a), g(b)) == map2(f(a), g(b))((_, _))
  //  (f(a), g(b)) == (f(a), g(b))
}

object Monad {
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {

    override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ma match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

    override def unit[A](a: => A): Either[E, A] = Right(a)
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_], G[_]](implicit F: Monad[F], G: Monad[G], T: Traverse[G]): Monad[({type f[x] = F[G[x]]})#f] =
    new Monad[({type f[x] = F[G[x]]})#f] {

      override def flatMap[A, B](ma: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
        F.flatMap(ma)(ga => F.map(T.traverse(ga)(f))(G.join))

      override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
    }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector())
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A, B, C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                                                            f: (A, B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] = new Applicative[({type f[x] = Validation[E, x]})#f] {

    override def unit[A](a: => A): Validation[E, _] = Success(a)

    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
      (fa, fb) match {
        case (Success(a), Success(b)) => Success(f(a, b))
        case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ (h2 +: t2))
        case (e@Failure(_, _), _) => e
        case (_, e@Failure(_, _)) => e
      }
  }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[Const[M, _]] =
    new Applicative[({type f[x] = Const[M, x]})#f] {
      def unit[A](a: => A): M = M.zero

      override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {

  type Id[A] = A

  implicit val idApplicative = new Applicative[Id] {
    override def unit[A](a: => A): Id[A] = a

    override def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] = f(fa, fb)
  }

  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]]

  def sequence[G[_] : Applicative, A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  def map[A, B](fa: F[A])(f: A => B): F[B] = traverse(fa)(a => idApplicative.unit(f(a)))

  import Applicative._

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B, x]})#f, A, Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _ <- set(s2)
    } yield b).run(s)

  def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] = mapAccum(fa, toList(fa).reverse)((_, s) => (s.head, s.tail))._1

  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, b) => ((), f(b, a)))._2

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
                            (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G product H)

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = {

    val self = this

    new Traverse[({type f[x] = F[G[x]]})#f] {
      override def traverse[M[_] : Applicative, A, B](fga: F[G[A]])(f: A => M[B]): M[F[G[B]]] =
        self.traverse(fga)(ga => G.traverse[M, A, B](ga)(a => f(a)))
    }
  }

}

object Traverse {

  val listTraverse = new Traverse[List] {
    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.unit(List[B]()))((a, gb) => G.map2(f(a), gb)(_ :: _))
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      fa match {
        case None => G.unit(None)
        case Some(a) => G.map(f(a))(Some(_))
      }
  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      fa match {
        case Tree(h, Nil) => G.map(f(h))(Tree(_, Nil))
        case Tree(h, t) => G.map2(f(h), listTraverse.traverse(t)(tree => traverse(tree)(f)))(Tree(_, _))
      }
  }
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}

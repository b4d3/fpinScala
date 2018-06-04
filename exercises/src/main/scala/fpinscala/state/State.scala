package fpinscala.state

import fpinscala.state.RNG.Rand

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {

    val (n, rng2) = rng.nextInt

    n match {
      case Int.MinValue => (0, rng2)
      case a if a < 0 => (-a, rng2)
      case a => (a, rng2)
    }
  }

  def double(rng: RNG): (Double, RNG) = {

    val (n, rng2) = nonNegativeInt(rng)
    val range = 1.toDouble / Int.MaxValue

    n match {
      case Int.MaxValue => (range * (Int.MaxValue - 1), rng2)
      case a => (range * a, rng2)
    }
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {

    val (randomInt, rng2) = rng.nextInt
    ((randomInt, double(rng)._1), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {

    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {

    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)

    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    @tailrec
    def loop(currentCount: Int, currentList: List[Int], currentRng: RNG): (List[Int], RNG) = {

      if (currentCount < 1) (currentList, currentRng)
      else {
        val (i, newRng) = currentRng.nextInt
        loop(currentCount - 1, i :: currentList, newRng)
      }
    }

    loop(count, Nil, rng)
  }

  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(_.toDouble / (Int.MaxValue + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {

    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(Nil: List[A]))((a, b) =>
    map2(a, b)(_ :: _))

  def intsViaSequence(count: Int)(rng: RNG): List[Int] = sequence(List.fill(count)(int))(rng)._1

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {

    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(i => {

    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  })

  def mapViaFlatmap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2ViaFlatmap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))


  def main(args: Array[String]): Unit = {

    println(double3(Simple(206026503483683L)))
    println(ints(5)(Simple(206026503483683L)))
    println(intsViaSequence(5)(Simple(206026503483683L)))
  }
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, s2) = run(s)
    (f(a), s2)
  })

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State(s => {
    val (a, s2) = run(s)
    val (b, s3) = sb.run(s2)
    (f(a, b), s3)
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })

  def get: State[S, S] = State(s => (s, s))

  def set(s: S): State[S, Unit] = State(_ => ((), s))

  def modify(f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  val int: State[RNG, Int] = (rng: RNG) => rng.nextInt

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](Nil: List[A]))((a, b) => a.map2(b)((a2, b2) => a2 :: b2))


  def processInput(input: Input): State[Machine, (Int, Int)] =
    State((m: Machine) => {

      (m, input) match {
        case (Machine(_, ca, co), _) if ca == 0 => ((co, ca), m)
        case (Machine(true, ca, co), Turn) => ((co, ca), m)
        case (Machine(false, ca, co), Coin) => ((co, ca), m)
        case (Machine(true, ca, co), Coin) if ca > 0 => ((co - 1, ca), Machine(false, ca, co + 1))
        case (Machine(false, ca, co), Turn) => ((co, ca - 1), Machine(true, ca - 1, co))
      }
    })


  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State(m => {

    val states = inputs.map(processInput)

    val (listOfCoinCandyValues, finalState) = sequence(states).run(m)

    ((finalState.coins, finalState.candies), finalState)
  })

  def main(args: Array[String]): Unit = {

    println(simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(Machine(true,
      5, 10)))
  }
}

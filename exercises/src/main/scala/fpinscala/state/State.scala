package fpinscala.state


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

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    (if (i < 0) -(i + 1) else i, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue + 1), r)
  }

  def doubleViaMap(rng: RNG): Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    1.to(count).foldRight((List(): List[Int], rng))((_, p) => {
      val (i, r) = p._2.nextInt
      (i :: p._1, r)
    })

  def intsRec(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(n: Int, res: (List[Int], RNG)): (List[Int], RNG) = {
      if (n == 0)
        res
      else {
        val (i, r) = res._2.nextInt
        go(n - 1, (i :: res._1, r))
      }
    }
    go(count, (List(), rng))
  }


  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r) = ra(rng)
      val (b, r2) = rb(r)
      (f(a,b), r2)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      fs.foldRight((List(): List[A], rng))((r, p) => {
        val (a, r1) = r(p._2)
        (a :: p._1, r1)
      })
    }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List(): List[A]))((r, res) => map2(r, res)(_ :: _))

  def intsViaSequence(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)((r: RNG) => r.nextInt))(rng)

  def intsViaSequence2(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence2(List.fill(count)((r: RNG) => r.nextInt))(rng)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r) = f(rng)
      g(a)(r)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + n - 1 >= mod) unit(mod) else nonNegativeLessThan(n)
    })

  def map_[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2_[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
}

import State._

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.reverse.foldLeft(unit[S, List[A]](List()))((acc, s) => {
      s.map2(acc)(_ :: _)
    })

  // just for checking sequence
  def ints(n: Int)(rng: RNG): (List[Int], RNG) =
    sequence[RNG, Int](List.fill(n)(State(_.nextInt))).run(rng)

  def get_[S]: State[S, S] = State(s => (s, s))

  def set_[S](s: S): State[S, Unit] = State(_ => ((), s))

  def simulate(i: Input): State[Machine, (Int, Int)] = i match {
    case Coin => State((s: Machine) =>
      if (s.locked && s.candies > 0)
        ((s.coins + 1, s.candies), Machine(false, s.candies, s.coins + 1))
      else
        ((s.coins, s.candies), s))
    case Turn => State((s: Machine) =>
      if (!s.locked && s.candies > 0)
        ((s.coins, s.candies - 1), Machine(true, s.candies - 1, s.coins))
      else
        ((s.coins, s.candies), s))
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    sequence(inputs.map(simulate(_))).map(_.last)
}

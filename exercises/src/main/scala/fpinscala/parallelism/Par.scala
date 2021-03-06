package fpinscala.parallelism

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)
  // `unit` is represented as a function that returns a `UnitFuture`, which is
  // a simple implementation of `Future` that just wraps a constant value.
  // It doesn't use the `ExecutorService` at all.
  // It's always done and can't be cancelled.
  // Its `get` method simply returns the value that we gave it.

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get 
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es) 
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  val x = lazyUnit(1)

  def f(s: ExecutorService) = fork(x)(s).get

  def fork[A](a: => Par[A]): Par[A] =
  // This is the simplest and most natural implementation of `fork`,
  // but there are some problems with it--for one,
  // the outer `Callable` will block waiting for the "inner" task to complete.
  // Since this blocking occupies a thread in our thread pool,
  // or whatever resource backs the `ExecutorService`,
  // this implies that we're losing out on some potential parallelism.
  // Essentially, we're using two threads when one should suffice.
  // This is a symptom of a more serious problem with the implementation,
  // and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] { 
      def call = a(es).get
    })

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

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

  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List()))((par, parl) => map2(par, parl)(_ :: _))

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val ls: List[Par[List[A]]] =
      as.map(asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(ls))(_.flatten)
  }

  def map3[A,B,C,D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A,B,C) => D): Par[D] = {
    val x = map2(pa, pb)((a, b) => f(a,b, _: C))
    map2(x, pc)((g, c) => g(c))
  }

  class ParOps[A](p: Par[A]) {
  }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => run(es)(choices(run(es)(n).get))
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int =
  // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library.
  // Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at
  // a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    // `headOption` is a method defined on all collections in Scala.// We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}

object Par2 {
  sealed trait Future[A] {
    protected[parallelism] def apply(cb: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val r = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es)(a => {
      try {
        r.set(a)
      } finally {
        latch.countDown()
      }
    })
    latch.await()
    r.get()
  }

  def unit[A](a: A): Par[A] =
    (_: ExecutorService) => {
      new Future[A] {
        def apply(cb: A => Unit): Unit = cb(a)
      }
    }

  def fork[A](p: => Par[A]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        eval(es)(p(es)(cb))
    }

  def eval[A](es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {
      def call = r
    })

  def map2[A,B,C](pa: Par[A], pb: Par[B])(f: (A,B) => C): Par[C] =
    es =>
      new Future[C] {
        def apply(cb: C => Unit) = {
          var oa: Option[A] = None
          var ob: Option[B] = None

          val combiner = Actor[Either[A, B]](es)({
            case Left(a) => ob match {
              case None => oa = Some(a)
              case Some(b) => eval(es)(cb(f(a,b)))
            }

            case Right(b) => oa match {
              case None => ob = Some(b)
              case Some(a) => eval(es)(cb(f(a,b)))
            }
          })

          pa(es)(a => combiner ! Left(a))
          pb(es)(b => combiner ! Right(b))
        }
      }
}
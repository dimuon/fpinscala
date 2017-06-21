package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def toList2: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], res: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: res)
      case _ => res.reverse
    }
    go(this, Nil)
  }

  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h,t) => buf += h(); go(t())
      case _ => buf.toList
    }

    go(this)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 0 => cons(h(),t().take(n - 1))
    case _ => empty
  }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((1, this))(p => p._2 match {
      case Cons(h,t) if p._1 <= n => Some(h(), (p._1 + 1, t()))
      case _ => None
    })

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_,t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

//  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
//    unfold((true, this))(p => p._2 match {

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def headOption2: Option[A] =
    foldRight[Option[A]](None)((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => cons(f(a), b))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h,t) => Some(f(h()), t())
      case _ => None
    }

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a,b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a) append b)

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def f(n: Int): Int = n match {
      case 0 => 0
      case 1 => 1
      case _ => f(n - 2) + f(n - 1)
    }
    def fi(n: Int): Stream[Int] =
      cons(f(n), fi(n + 1))
    fi(0)
  }

  def fibs2: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a,s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  def unfoldViaMap[A, S](z: S)(f: S => Option[(A,S)]): Stream[A] =
    f(z).map((p: (A,S)) => cons(p._1, unfoldViaMap(p._2)(f))).getOrElse(empty[A])

  def unfoldViaFold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).fold(empty[A])((p: (A, S)) => cons(p._1, unfoldViaFold(p._2)(f)))

  def onesViaUnfold: Stream[Int] =
    unfold(1)(_ => Some((1, 1)))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a,a)))

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(i => Some((i, i + 1)))

  def fibsViaUnfold: Stream[Int] =
    unfold((0,1))(p => Some(p._1, (p._2, p._1 + p._2)))
}
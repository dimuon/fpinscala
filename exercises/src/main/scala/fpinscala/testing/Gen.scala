package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  def check(tag: String): Boolean

//  def &&(p: Prop): Prop = {
//    val that = this
//    new Prop {
//      def check = that.check("left") && p.check("right")
//    }
//  }

}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

case class Gen2[A](sample: State[RNG,A]) {
  def flatMap[B](f: A => Gen2[B]): Gen2[B] =
    Gen2(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen2[Int]): Gen2[List[A]] =
    size.flatMap(s => Gen2(State.sequence(List.fill(s)(sample))))

//  def listOfN(size: Gen2[Int]): SGen[List[A]] =
//    size.flatMap(s => Gen2(State.sequence(List.fill(s)(sample))))
}

object Gen2 {
  def choose(start: Int, stopExclusive: Int): Gen2[Int] =
    Gen2(State(_.nextInt match {
      case (n, rng) => (start + Math.abs(n) % (stopExclusive - start), rng)
    }))

  def unit[A](a: => A): Gen2[A] = Gen2(State(s => (a, s)))

  def boolean: Gen2[Boolean] = Gen2(State(_.nextInt match {
    case (n, rng) => (n > 0, rng)
  }))

//  def listOfN[A](n: Int, g: Gen2[A]): Gen2[List[A]] =
//    Gen2(State.sequence(List.fill(n)(g.sample)))

//  def listOfN[A](g: Gen2[A]): SGen[List[A]] =
//    SGen(n => Gen2(State.sequence(List.fill(n)(g.sample))))

  def pair(start: Int, stopExclusive: Int): Gen2[(Int, Int)] = {
    val (g1, g2) = (choose(start, stopExclusive), choose(start, stopExclusive))
    Gen2(g1.sample.map2[Int,(Int,Int)](g2.sample)((i1, i2) => (i1, i2)))
  }

  def genOpt[A](g: Gen2[A]): Gen2[Option[A]] =
    Gen2(g.sample.map(Some(_)))

  def genFromOpt[A](g: Gen2[Option[A]]): Gen2[A] =
    Gen2(g.sample.map(_.get))

  def union[A](g1: Gen2[A], g2: Gen2[A]): Gen2[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen2[A], Double), g2: (Gen2[A], Double)): Gen2[A] = {
    val threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen2(State(RNG.double).flatMap(d =>
      if (d < threshold) g1._1.sample else g2._1.sample))
  }

}

object Gen {
  def unit[A](a: => A): Gen[A] = ???
}

trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???

//  def unsized: SGen[A] = SGen(_ => this)
}

//case class SGen[+A](forSize: Int => Gen2[A])

//trait SGen[+A] {
//
//}


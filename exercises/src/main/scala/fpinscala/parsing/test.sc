import fpinscala.parsing._

trait Par

import language.higherKinds

object test {
  import fpinscala.parsing.Parsers

  val s1 = new Parser[String]
  val s2 = new Parser[String]

  val s = s1.or
}
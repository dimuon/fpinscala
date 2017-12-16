package fpinscala.parsing

import language.higherKinds

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait
  def or[A] (p1: Parser[A], p2: Parser[A]) = p1
//  implicit def parsers[A](p: Parser[A]): ParserOps[A] = ParserOps(p)

//  case class ParserOps[A](p: Parser[A]) {
//    def |[B >: A] (p2: Parser[B]) = self.or(p, p2)
//    def or[B >: A] (p2: Parser[B]) = self.or(p, p2)
//
//    def map[A,B](a: Parser[A])(f: A => B): Parser[B]
//  }

  object Laws {
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}

class Parser[A] {

}

class TrivialParsers[Parser[+_]] extends Parsers[Parser] {
}
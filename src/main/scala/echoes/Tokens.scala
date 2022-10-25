/**
  * Apache License
  * Version 2.0, January 2004
  * http://www.apache.org/licenses/
  */

package echoes

sealed abstract class Token {
  lazy val dbgString: String = this match {
    case Identifier(name) => s"ID($name)"
    case Keyword(name) => s"KEYWORD($name)"
    case Literal(value) => s"LIT($value)"
    case Punctuation(op) => s"PUNC($op)"
  }
}

final case class Punctuation(op: String) extends Token
final case class Keyword(name: String) extends Token
final case class Identifier(name: String) extends Token
final case class Literal(value: LiteralValue) extends Token

sealed abstract class LiteralValue
final case class IntLiteral(value: Int) extends LiteralValue {
  override def toString(): String = value.toString()
}

final case class BoolLiteral(value: Boolean) extends LiteralValue {
  override def toString(): String = value.toString()
}

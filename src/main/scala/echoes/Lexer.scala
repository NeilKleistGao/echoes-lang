package echoes

final class Lexer(source: SourceFile, dbg: Boolean) {
  type LexResult = (List[Token], Diagnostic)

  def lex: LexResult = {
    val diag = new Diagnostic(source.path)

    source.readContent match {
      case Left(value) => {
        val size = value.length()
        def go(pointer: Int, prev: LexResult): LexResult =
          if (pointer == size) prev
          else {
            val ch = value(pointer)
            if (Punctuation.isPunctuation(ch)) {
              debug(Punctuation(ch).dbgString)
              go(pointer + 1, (prev._1 :+ Punctuation(ch), prev._2))
            }
            else {
              def takeWhile(current: Int)(implicit condition: Char => Boolean): Int = 
                if (current >= size || !condition(value(current))) current
                else takeWhile(current + 1)
              
              if (IntLiteral.isNumber(ch)) {
                val end = takeWhile(pointer + 1)(IntLiteral.isNumber)
                val lit = Literal(IntLiteral(value.substring(pointer, end).toInt))
                debug(lit.dbgString)
                go(end, (prev._1 :+ lit, prev._2))
              }
              else if (ch == ' ' || ch == '\n' || ch == '\t')
                go(pointer + 1, prev)
              else {
                val end = takeWhile(pointer + 1)(c =>
                  (!Punctuation.isPunctuation(c) && c != ' ' && c != '\n' && c != '\t'))
                val str = value.substring(pointer, end)
                if (BoolLiteral.isBool(str)) {
                  val lit = Literal(BoolLiteral(str.equals("true")))
                  debug(lit.dbgString)
                  go(end, (prev._1 :+ lit, prev._2))
                }
                else if (Keyword.isKeyword(str)) {
                  debug(Keyword(str).dbgString)
                  go(end, (prev._1 :+ Keyword(str), prev._2))
                }
                else {
                  debug(Identifier(str).dbgString)
                  go(end, (prev._1 :+ Identifier(str), prev._2))
                }
              }
            }
          }

        go(0, (List(), diag))
      }
      case Right(value) => (List(), diag.error(value, Location.EmptyLocation))
    }
  }

  private def debug(msg: String): Unit = 
    if (dbg) System.out.println(s"[lexer debug]: $msg")
}

object Lexer {
  def apply(source: SourceFile, dbg: Boolean) = new Lexer(source, dbg)
}

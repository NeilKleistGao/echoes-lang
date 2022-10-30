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
            if (Punctuation.isPunctuation(ch))
              go(pointer + 1, (prev._1 :+ Punctuation(ch), prev._2))
            else {
              def takeWhile(current: Int)(implicit condition: Char => Boolean): Int = 
                if (current >= size || !condition(value(current))) current
                else takeWhile(current)
              
              if (IntLiteral.isNumber(ch)) {
                val end = takeWhile(pointer + 1)(IntLiteral.isNumber)
                go(end, (prev._1 :+ Literal(IntLiteral(value.substring(pointer, end).toInt)), prev._2))
              }
              else {
                val end = takeWhile(pointer + 1)(c =>
                  !Punctuation.isPunctuation(c) && c != ' ' && c != '\n' && c != '\t')
                val str = value.substring(pointer, end)
                if (BoolLiteral.isBool(str))
                  go(end, (prev._1 :+ Literal(BoolLiteral(str.equals("true"))), prev._2))
                else if (Keyword.isKeyword(str))
                  go(end, (prev._1 :+ Keyword(str), prev._2))
                else
                  go(end, (prev._1 :+ Identifier(str), prev._2))
              }
            }
          }

        go(0, (List(), diag))
      }
      case Right(value) => (List(), diag.error(value, Location.EmptyLocation))
    }
  }
}

object Lexer {
  def apply(source: SourceFile, dbg: Boolean) = new Lexer(source, dbg)
}

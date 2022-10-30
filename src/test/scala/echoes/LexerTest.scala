/**
  * Apache License
  * Version 2.0, January 2004
  * http://www.apache.org/licenses/
  */

package echoes

import org.scalatest.funsuite.AnyFunSuite

class LexerTest extends AnyFunSuite {
  test("Lexer Test") {
    import java.io.{File, PrintWriter}
    val sourceFile = SourceFile("src/test/seed/lexer/demo.seed")
    val lexer = Lexer(sourceFile, true)
    val res = lexer.lex

    assert(!res._2.isFailed)
    assert(res._1.length > 0)
    val writer = new PrintWriter(new File("src/test/tokens/demo.txt"))
    res._1.foreach(tk => writer.write(tk.dbgString + "\n"))
    writer.close()
  }

  test("Undefined Test") {
    val sourceFile = SourceFile("src/test/seed/lexer/undefined.seed")
    val lexer = Lexer(sourceFile, true)
    val res = lexer.lex

    assert(res._1.isEmpty)
    assert(res._2.isFailed)
    assert(res._2.path.equals("src/test/seed/lexer/undefined.seed"))
    assert(res._2.messageList.length == 1)
  }
}


/**
  * Apache License
  * Version 2.0, January 2004
  * http://www.apache.org/licenses/
  */

package echoes

/**
  * The data of seed files.
  *
  * @param path the absolute path of the file
  */
abstract class SourceFile(val path: String) {
  lazy val filename = path.substring(path.lastIndexOf("/") + 1)
  def readContent: Either[String, String] = ???

  def quote(content: String) = new SourceFile(path) {
    override def readContent: Either[String, String] = Left(content)
  }
}

object SourceFile {
  def apply(path: String) = new SourceFile(path) {
    override def readContent: Either[String, String] = {
      try {
        val source = scala.io.Source.fromFile(path, "UTF-8")
        val lines = source.getLines()
        Left(lines.foldLeft("")((res, line) => res + line + "\n"))
      }
      catch {
        case _: Throwable => Right(s"can't open the file $path")
      }
    }
  }
}

class Location(line: Int, begin: Int, end: Int)
object Location {
  def apply(line: Int, begin: Int, end: Int) =
    new Location(line, begin, end)

  val EmptyLocation = Location(0, 0, 0)
}

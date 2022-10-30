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
    import java.io.{BufferedReader, FileReader, IOException}

    override def readContent: Either[String, String] = {
      val reader =
        try { new BufferedReader(new FileReader(path)) }
        catch {
          case e: IOException => null
        }

      if (reader == null) Right(s"can't open the file $path")
      else {
        def read(reader: BufferedReader, prev: String): String = {
          val line = reader.readLine()
          if (line == null) prev
          else read(reader, s"$prev$line\n")
        }

        Left(read(reader, ""))
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

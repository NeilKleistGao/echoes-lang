package echoes

final class Diagnostic(val path: String, val messageList: List[Diagnostic.SubMessage] = List()) {
  import Diagnostic._
  def error(msg: String, loc: Location) =
    new Diagnostic(path, messageList :+ new SubMessage(msg, loc, Error))
  
  def warn(msg: String, loc: Location) =
    new Diagnostic(path, messageList :+ new SubMessage(msg, loc, Warning))

  def message(msg: String, loc: Location) =
    new Diagnostic(path, messageList :+ new SubMessage(msg, loc, Message))

  def isFailed = messageList.foldLeft(false)((res, d) => res || (d match {
    case SubMessage(_, _, Error) => true
    case _ => false
  }))
}

object Diagnostic {
  sealed trait DiagnosticType
  final case object Error extends DiagnosticType
  final case object Warning extends DiagnosticType
  final case object Message extends DiagnosticType

  final case class SubMessage(msg: String, loc: Location, tp: DiagnosticType)
}

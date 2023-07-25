import optics.Prism

object TestPrism {
  sealed trait Json
  case object JNull extends Json
  case class JStr(v: String) extends Json
  case class JNum(v: Double) extends Json
  case class JObj(v: Map[String, Json]) extends Json

  val jStr: Prism[Json, String] = Prism[Json, String] {
    case JStr(v) => Some(v)
    case _       => None
  }(JStr)

  def main(args: Array[String]): Unit = {
    assert(jStr.reverseGet("hello") == JStr("hello"))
    assert(jStr.getOption(JStr("Hello")) == Some("Hello"))
    assert(jStr.modify(_.reverse)(JStr("Hello")) == JStr("olleH"))
    assert(jStr.modify(_.toUpperCase)(JNum(234)) == JNum(234))
  }
}

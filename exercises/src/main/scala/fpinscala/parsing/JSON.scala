package fpinscala.parsing

trait JSON

object JSON {

  case object JNull extends JSON

  case class JNumber(get: Double) extends JSON

  case class JString(get: String) extends JSON

  case class JBool(get: Boolean) extends JSON

  case class JArray(get: IndexedSeq[JSON]) extends JSON

  case class JObject(get: Map[String, JSON]) extends JSON


  def jsonParser[ParseError, Parser[+ _]](P: Parsers[ParseError, Parser]): Parser[JSON] = {
    import P._

    def pair(p: Parser[JSON]): Parser[(String, JSON)] =
      (stringLiteral.skipWhitespaces ** char(':')).map(_._1) ** p.skipWhitespaces

    def stringLiteral: Parser[String] = regex("\"[^\"]+\"".r)

    def jnull: Parser[JSON] = nil map(n => JNull)

    def jnumber: Parser[JSON] = double map (d => JNumber(d))

    def jstring: Parser[JSON] = stringLiteral map(s => JString(s))

    def jbool: Parser[JSON] = bool map JBool

    def literal: Parser[JSON] = jstring | jnull | jnumber | jbool

    def jarray: Parser[JSON] = surround(char('['), extractSeparatedValues(char(','), value), char(']')) map (l => JArray(l.toIndexedSeq))

    def value: Parser[JSON] = jobject | literal | jarray

    def jobject: Parser[JSON] =
      surround(char('{'), extractSeparatedValues(char(','), pair(value)), char('}')) map(l =>
        JObject(l.toMap))

    whitespace *> (jarray | jobject).skipWhitespaces
  }
}

package catsParse



  
import cats.parse.{Parser0, Parser => P, Numbers}
import org.typelevel.jawn.ast._

object Json {
  private[this] val whitespace: P[Unit] = P.charIn(" \t\r\n").void
  private[this] val whitespaces0: P[Unit] = whitespace.rep.void

  val parser: P[JValue] = P.recursive[JValue] { recurse =>
    val pnull = P.string("null").as(JNull)
    val bool = P.string("true").as(JBool.True).orElse(P.string("false").as(JBool.False))
    val justStr = JsonStringUtil.escapedString('"')
    val str = justStr.map(JString(_))
    val num = Numbers.jsonNumber.map(JNum(_))

    val listSep: P[Unit] =
      P.char(',').surroundedBy(whitespaces0).void

    def rep[A](pa: P[A]): Parser0[List[A]] =
      pa.repSep0(listSep).surroundedBy(whitespaces0)

    val list = rep(recurse).with1
      .between(P.char('['), P.char(']'))
      .map { vs => JArray.fromSeq(vs) }

    val kv: P[(String, JValue)] =
      justStr ~ (P.char(':').surroundedBy(whitespaces0) *> recurse)

    val obj = rep(kv).with1
      .between(P.char('{'), P.char('}'))
      .map { vs => JObject.fromSeq(vs) }

    P.oneOf(str :: num :: list :: obj :: bool :: pnull :: Nil)
  }

  // any whitespace followed by json followed by whitespace followed by end
  val parserFile: P[JValue] = whitespaces0.with1 *> parser <* (whitespaces0 ~ P.end)
}

  

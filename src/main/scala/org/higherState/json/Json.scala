package org.higherState.json

sealed trait Json extends Any

trait JNull extends Json{
  def value = null
}
object JNull extends JNull

case class JNumber(n:Number) extends AnyVal with Json
case class JString(s:String) extends AnyVal with Json
case class JBool(value:Boolean) extends AnyVal with Json
case class JArray(value:Seq[Json]) extends AnyVal with Json
case class JObject(value:JMap) extends AnyVal with Json

object JObject {
  def apply(values:(String, Json)*) =
    JObject(values.toMap)
}



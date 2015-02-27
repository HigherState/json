package org.higherState.json

sealed trait Json extends Any {
}

trait JNull extends Json{
  def value = null
}
object JNull extends JNull

case class JLong(value:Long) extends AnyVal with Json
case class JDouble(value:Double) extends AnyVal with Json
case class JString(value:String) extends AnyVal with Json
case class JBool(value:Boolean) extends AnyVal with Json
case class JArray(value:Seq[Json]) extends AnyVal with Json
case class JObject(value:JMap) extends AnyVal with Json

object JObject {
  def apply(values:(String, Json)*):JObject =
    JObject(values.toMap)
  val empty = JObject(Map.empty[String, Json])
}
object JArray {
  def apply(value:Json, values:Json*):JArray =
    JArray(value +: values)
  val empty = JArray(Seq.empty)
}



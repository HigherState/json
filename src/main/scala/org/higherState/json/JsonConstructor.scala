package org.higherState.json

trait JsonConstructor {

  implicit class StringtoJ(val value:String) extends AnyVal {
    def j = JString(value)
    def \ = Path(Vector(Left(value)))
  }
  implicit class MaptoJ(val value:Map[String,Json]) extends AnyVal {
    def j = JObject(value)
  }
  implicit class TupleItertoJ(val value:TraversableOnce[(String,Json)]) extends AnyVal {
    def j = JObject(value.toMap)
  }
  implicit class InttoJ(val value:Int) extends AnyVal {
    def j = JNumber(value)
  }
  implicit class NumberToJ(val value:Number) extends AnyVal {
    def j = JNumber(value)
  }
  implicit class BooltoJ(val value:Boolean) extends AnyVal {
    def j = JBool(value)
  }

  implicit class SeqtoJ(val value:Seq[Json]) extends AnyVal {
    def j = JArray(value)
  }
}

object JsonConstructor extends JsonConstructor

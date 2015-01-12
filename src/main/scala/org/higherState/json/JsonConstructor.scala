package org.higherState.json

object JsonConstructor {

  implicit class StringtoJ(val value:String) extends AnyVal {
    def j:Json = JString(value)
    def \ = Path(Vector(Left(value)))
  }
  implicit class MaptoJ(val value:Map[String,Json]) extends AnyVal {
    def j:Json  = JObject(value)
  }
  implicit class TupleItertoJ(val value:TraversableOnce[(String,Json)]) extends AnyVal {
    def j:Json  = JObject(value.toMap)
  }
  implicit class InttoJ(val value:Int) extends AnyVal {
    def j:Json  = JLong(value)
  }
  implicit class LongtoJ(val value:Long) extends AnyVal {
    def j:Json  = JLong(value)
  }
  implicit class FloattoJ(val value:Float) extends AnyVal {
    def j:Json  = JDouble(value)
  }
  implicit class DoubletoJ(val value:Double) extends AnyVal {
    def j:Json  = JDouble(value)
  }
  implicit class BooltoJ(val value:Boolean) extends AnyVal {
    def j:Json  = JBool(value)
  }

  implicit class SeqtoJ(val value:Seq[Json]) extends AnyVal {
    def j:Json  = JArray(value)
  }
}

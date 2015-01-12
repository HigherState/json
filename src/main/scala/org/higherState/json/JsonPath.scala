package org.higherState.json

object JsonPath {

  implicit class JPath(val json:Json) extends AnyVal {
    def \(key:String):Option[Json] =
      json match {
        case obj:JObject => obj.value.get(key)
        case _ => None
      }
    def \(key:Int):Option[Json] =
      json match {
        case array:JArray if key < array.value.size =>
          Some(array.value(key))
        case _ => None
      }
  }

  implicit class JMaybePath(val json:Option[Json]) extends AnyVal {
    def \(key:String):Option[Json] =
      json.flatMap(_\key)
    def \(key:Int):Option[Json] =
      json.flatMap(_\key)
  }
}

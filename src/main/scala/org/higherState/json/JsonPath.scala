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
    def \(path:Path):Option[Json] =
      getValue(json, path.segments)
  }

  implicit class JMaybePath(val json:Option[Json]) extends AnyVal {
    def \(key:String):Option[Json] =
      json.flatMap(_\key)
    def \(key:Int):Option[Json] =
      json.flatMap(_\key)
    def \(path:Path):Option[Json] =
      json.flatMap(_\path)
  }

  def getValue(target:Json, segments:Segments):Option[Json] =
    (segments, target) match {
      case (Vector(), _) =>
        Some(target)
      case (Left(head) +: tail, JObject(obj)) =>
        obj.get(head).flatMap(getValue(_, tail))
      case (Right(head) +: tail, JArray(array)) =>
        if (head < array.size)
          Some(array(head)).flatMap(getValue(_, tail))
        else
          None
      case _ => None
    }
}

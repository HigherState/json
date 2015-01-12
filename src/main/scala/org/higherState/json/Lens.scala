package org.higherState.json

trait Lens[T] {
  def apply(obj:Json)(value:T):Json
}

object Lens {

  def getValue(target:Json, segments:Segments):Option[Json] =
    (segments, target) match {
      case (Vector(), _) =>
        Some(target)
      case (Left(head) +: tail, obj:JObject) =>
        obj.value.get(head).flatMap(getValue(_, tail))
      case (Right(head) +: tail, array:JArray) =>
        if (head < array.value.size)
          Some(array.value(head))
        else
          None
      case _ => None
    }

  def setValue(target:Option[Json], segments:Segments, value:Json):Json =
    (segments, target) match {
      case (Left(key) +: tail, Some(obj:JObject)) =>
        JObject(obj.value + (key -> setValue(obj.value.get(key), tail, value)))
      case (Left(key) +: tail, _) =>
        JObject(key -> setValue(None, tail, value))
      case (Right(index) +: tail, Some(array:JArray)) =>
        val (left, right) = array.value.splitAt(index)
        val leftPad =
        if (left.size < index)
          left.padTo(index, JNull)
        else left
          JArray((leftPad :+ setValue(right.headOption, tail, value)) ++ right)
      case (Right(index) +: tail, _) =>
        JArray(Seq.fill(index)(JNull) :+ setValue(None, tail, value))
      case _ =>
        value
    }

  def removeValue(target:Json, segments:Segments):Json =
    (segments, target) match {
      case (Left(key) +: Vector(), obj:JObject) =>
        JObject(obj.value - key)
      case (Left(key) +: tail, obj:JObject) =>
        obj.value.get(key).fold(obj)(v => JObject(obj.value + (key -> removeValue(v, tail))))
      case _ =>
        target
    }
}
object Compositor {
  implicit class F(val f: Json => Json) extends AnyVal {
    def ~(f2: Json => Json): Json => Json =
      (j: Json) => f2(f(j))
  }
}
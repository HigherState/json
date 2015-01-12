package org.higherState.json

trait Lens[T] {
  def apply(obj:Json)(value:T):Json
}

object Lens {

  def getValue(target:Json, segments:Segments):Option[Json] =
    (segments, target) match {
      case (Vector(), _) =>
        Some(target)
      case (Left(head) +: tail, JObject(obj)) =>
        obj.get(head).flatMap(getValue(_, tail))
      case (Right(head) +: tail, JArray(array)) =>
        if (head < array.size)
          Some(array(head))
        else
          None
      case _ => None
    }

  def setValue(target:Option[Json], segments:Segments, value:Json):Json =
    (segments, target) match {
      case (Left(key) +: tail, Some(JObject(obj))) =>
        JObject(obj + (key -> setValue(obj.get(key), tail, value)))
      case (Left(key) +: tail, _) =>
        JObject(key -> setValue(None, tail, value))
      case (Right(index) +: tail, Some(JArray(array))) =>
        val (left, right) = array.splitAt(index)
        if (left.size < index)
          JArray(left.padTo(index, JNull) :+ setValue(None, tail, value))
        else
          JArray((left :+ setValue(right.headOption, tail, value)) ++ right.tail)
      case (Right(index) +: tail, _) =>
        JArray(Seq.fill(index)(JNull) :+ setValue(None, tail, value))
      case _ =>
        value
    }

  def removeValue(target:Json, segments:Segments):Json =
    (segments, target) match {
      case (Left(key) +: Vector(), JObject(obj)) =>
        JObject(obj - key)
      case (Left(key) +: tail, JObject(obj)) =>
        JObject(obj.get(key).fold(obj)(v => obj + (key -> removeValue(v, tail))))
      case (Right(index) +: tail, JArray(seq)) =>
        ???
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
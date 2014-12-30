package org.higherState.json

trait Lens[T] {
  def apply(obj:JType)(value:T):JType
}

object Lens {

  def getValue(target:JType, segments:Segments):Option[JType] =
    (segments, target) match {
      case (Vector(), _) => Some(target)
      case (Left(head) +: tail, j:JObject) => j.get(head).flatMap(getValue(_, tail))
      case (Right(head) +: tail, j:JArray) => j \ head
      case _ => None
    }

  def setValue(target:Option[JType], segments:Segments, value:JType):JType =
    (segments, target) match {
      case (Left(key) +: tail, Some(obj:JObject)) =>
        obj + (key -> setValue(obj.get(key), tail, value))
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

  def removeValue(target:JType, segments:Segments):JType =
    (segments, target) match {
      case (Left(key) +: Vector(), obj:JObject) =>
        obj - key
      case (Left(key) +: tail, obj:JObject) =>
        obj.get(key).fold(obj)(v => obj + (key -> removeValue(v, tail)))
      case _ =>
        target
    }
}
object Compositor {
  implicit class F(val f: JType => JType) extends AnyVal {
    def ~(f2: JType => JType): JType => JType =
      (j: JType) => f2(f(j))
  }
}
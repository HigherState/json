package org.higherState.json

import scalaz.Scalaz._

object JsonFunctions {
  /**
   * Concatenate and replace json structure with delta, where a JNull or Empty JObject value will clear the key value pair
   * @param delta
   * @return
   */
  def applyDelta(target:Json, delta:Json):Json =
    (target, delta) match {
      case (t:JObject, d:JObject) =>
        applyObjectDelta(t,d)
      case _ =>
        delta
    }

  def difference(delta:Json, source:Json):Option[Json] =
    (delta, source) match {
      case (d, s) if d == s =>
        None
      case (d:JObject, s:JObject) =>
        val o = d.value.flatMap { kvp =>
          s.value.get(kvp._1).fold(Option(kvp)){ v =>
            difference(kvp._2, v).map(kvp._1 -> _)
          }
        }
        o.nonEmpty.option(JObject(o))
      case (d, _) =>
        Some(d)
    }

  def setValue(target:Option[Json], segments:Segments, value:Json, insert:Boolean = false):Json =
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

  def insertValue(target:Option[Json], segments:Segments, value:Json):Json =
    (segments, target) match {
      case (Left(key) +: tail, Some(JObject(obj))) =>
        JObject(obj + (key -> setValue(obj.get(key), tail, value)))
      case (Left(key) +: tail, _) =>
        JObject(key -> setValue(None, tail, value))
      case (Right(index) +: tail, Some(JArray(array))) =>
        val (left, right) = array.splitAt(index)
        if (left.size < index)
          JArray(left.padTo(index, JNull) :+ setValue(None, tail, value))
        else if (tail.isEmpty)
          JArray((left :+ value) ++ right)
        else
          JArray((left :+ setValue(right.headOption, tail, value)) ++ right.tail)
      case (Right(index) +: tail, _) =>
        JArray(Seq.fill(index)(JNull) :+ setValue(None, tail, value))
      case _ =>
        value
    }

  def dropValue(target:Json, segments:Segments):Json =
    (segments, target) match {
      case (Left(key) +: Vector(), JObject(obj)) =>
        JObject(obj - key)
      case (Left(key) +: tail, JObject(obj)) =>
        JObject(obj.get(key).fold(obj)(v => obj + (key -> dropValue(v, tail))))
      case (Right(index) +: Vector(), JArray(seq)) if index < seq.length =>
        val (left, right) = seq.splitAt(index)
        JArray(left ++ right.drop(1))
      case (Right(index) +: tail, JArray(seq)) if index < seq.length =>
        val (left, right) = seq.splitAt(index)
        JArray((left :+ dropValue(right.head, tail)) ++ right.drop(1))
      case _ =>
        target
    }

  private def applyObjectDelta(target:JObject, delta:JObject):JObject =
    JObject(
      delta.value.foldLeft(target.value) { (m, kvp) =>
        (m.get(kvp._1), kvp._1, kvp._2) match {
          case (Some(j:JObject), key, d:JObject) =>
            val objectDelta = applyObjectDelta(j, d)
            if (objectDelta.value.isEmpty) m - key
            else m + (key -> objectDelta)
          case (_, key, JNull) =>
            m - key
          case (Some(j), _, newValue) if j == newValue =>
            m
          case (_, key, newValue) =>
            m + (key -> newValue)
        }
      }
    )

  /**
   * Concatenate and replace json structure with delta
   * @param delta
   * @return
   */
  def mergeDelta(target:Json, delta:Json):Json =
    (target, delta) match {
      case (t:JObject, d:JObject) =>
        mergeObjectDelta(t,d)
      case _ =>
        delta
    }
  private def mergeObjectDelta(target:JObject, delta:JObject):JObject =
    JObject(
      delta.value.foldLeft(target.value) { (m, kvp) =>
        (m.get(kvp._1), kvp._1, kvp._2) match {
          case (Some(j:JObject), key, d:JObject) =>
            m + (key -> mergeObjectDelta(target, delta))
          case (Some(j), _, newValue) if j == newValue =>
            m
          case (_, key, newValue) =>
            m + (key -> newValue)
        }
      }
    )
}

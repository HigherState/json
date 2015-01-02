package org.higherState.json

trait JsonFunctions {
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

  private def applyObjectDelta(target:JObject, delta:JObject):JObject =
    JObject(
      delta.value.foldLeft(target.value) { (m, kvp) =>
        (m.get(kvp._1), kvp._1, kvp._2) match {
          case (Some(j:JObject), key, d:JObject) =>
            val objectDelta = applyObjectDelta(j, d)
            if (objectDelta.isEmpty) m - key
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

package org.higherState.json

import scala.collection.GenTraversableOnce

trait JObject extends Any with JType {

  def value:Map[String, JType]

  def apply(key:String) =
    value(key)

  override def \(key:String) =
    value.get(key)

  def + (keyValuePair:(String, JType)):JObject =
    JObject(value + keyValuePair)

  def + (elem1: (String, JType), elem2: (String, JType), elems: (String, JType) *) =
    JObject(value + (elem1, elem2, elems:_*))

  def +? (keyValuePair:(String, Option[JType])):JObject =
    keyValuePair._2.fold(this)(value => this + (keyValuePair._1 -> value))

  def ++ (keyValuePairs:TraversableOnce[(String, JType)]):JObject =
    JObject(value ++ keyValuePairs)

  def ++ (jObject:JObject):JObject =
    JObject(value ++ jObject.value)

  def - (key:String):JObject =
    JObject(value - key)

  def - (key1:String, key2:String, keys:String*):JObject =
    JObject(value - (key1, key2, keys:_*))

  def -- (keys:TraversableOnce[String]) =
    JObject(value -- keys)

  def map[T](func:((String,JType)) => T) =
    value.map(func)

  def flatMap[T](func:((String,JType)) => GenTraversableOnce[T]):TraversableOnce[T] =
    value.flatMap(func)

  def filter(func:((String,JType)) => Boolean) =
    JObject(value.filter(func))

  def isEmpty = value.isEmpty

  def nonEmpty = value.nonEmpty

  def collect[T](pf:PartialFunction[(String, JType), T]) =
    value.collect(pf)

  /**
   * Concatenate and replace json structure with delta, where a JNull value will clear the key value pair.
   * @param delta
   * @return
   */
  def applyDelta(delta:JObject):JObject =
    JObject(
      delta.value.foldLeft(value) { (m, kvp) =>
        (m.get(kvp._1), kvp._1, kvp._2) match {
          case (Some(j:JObject), key, d:JObject) =>
            val objectDelta = j applyDelta d
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
  def mergeDelta(delta:JObject):JObject =
    JObject(
      delta.value.foldLeft(value) { (m, kvp) =>
        (m.get(kvp._1), kvp._1, kvp._2) match {
          case (Some(j:JObject), key, d:JObject) =>
            m + (key -> (j mergeDelta d))
          case (Some(j), _, newValue) if j == newValue =>
            m
          case (_, key, newValue) =>
            m + (key -> newValue)
        }
      }
    )

  def get(key:String):Option[JType] =
    value.get(key)

  def get(path:Path):Option[JType] =
    path.parts match {
      case head :: Nil =>
        get(head)
      case head :: tail =>
        get(head).asJObject.flatMap(_.get(Path(tail)))
      case Nil =>
        Some(this)
    }

  def getOrElse(key:String, orElse: => JType) =
    value.getOrElse(key, orElse)

  /**
   * Returns those elements in the object which are different, or not contained in the parameter.
   * @param obj Json object to compare against
   * @return
   */
  def difference(obj:JObject):JObject =
    value.foldLeft(value){ (m, kvp) =>
      (kvp._2, obj.get(kvp._1)) match {
        case (JNull, None) =>
          m - kvp._1
        case (a:JObject, Some(b:JObject)) =>
          val child = a.difference(b)
          if (child.isEmpty) m - kvp._1
          else m + (kvp._1 -> child)
        case (a:JObject, _) =>
          val child = a.difference(JObject.empty)
          if (child.isEmpty) m - kvp._1
          else m + (kvp._1 -> child)
        case (a, Some(b)) if a == b =>
          m - kvp._1
        case _ =>
          m
      }
    }.j

  def contains(key:String):Boolean =
    value.contains(key)

  def size =
    value.size

}

private case class JObjectImpl(value:Map[String, JType]) extends AnyVal with JObject

object JObject {
  def apply(map:Map[String, JType]):JObject =
    JObjectImpl(map)

  def apply(elems: (String, JType) *):JObject =
    JObjectImpl(Map(elems:_*))

  def unapply(obj:Any) =
    obj match {
      case j:JObject => Some(j.value)
      case _ => None
    }

  val empty:JObject =
    JObjectImpl(Map.empty[String, JType])
}

case class Path(parts:List[String]) extends AnyVal {
  def \(part:String) = Path(parts ++ List(part))

  def apply(part:String) = Path(parts ++ List(part))
}

object Path {
  val empty = Path(Nil)
}
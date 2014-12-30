package org.higherState.json

case class JArray(value:Seq[JType]) extends AnyVal with JType {
  def collect[T](pf:PartialFunction[JType,T]) =
    value.collect(pf)
  def map(f:JType => JType) =
    value.map(f).j
  def size = value.size

  def :+(x:JType) =
    JArray(value :+ x)
  def \ (key:Int):Option[JType] =
    if (key < value.size) Some(value(key))
    else None
}

object JArray {

  def apply(elem:JType, elems:JType*):JArray =
    JArray(elem +: elems)

  val empty = JArray(Seq.empty[JType])
}

case class JIter(value:TraversableOnce[JType]) extends AnyVal with JType

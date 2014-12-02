package org.higherState.json


case class Path(parts:Vector[String]) extends AnyVal {
  def \(part:String) = Path(parts :+ part)

  def apply(jType:JType):Option[JType] =
    (parts, jType) match {
      case (Vector(), _) => Some(jType)
      case (head +: tail, j:JObject) => j.get(head).flatMap(Path(tail)(_))
      case _ => None
    }

  override def toString = parts.mkString("\\")
}

object Path {
  val empty = Path(Vector.empty)

  def apply(parts:String*):Path = Path(parts.toVector)
}
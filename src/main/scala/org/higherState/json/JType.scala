package org.higherState.json

trait JType extends Any {
  def value:Any

  def apply[B](pf:PartialFunction[JType, B]) =
    pf.lift.apply(this)

  def \ (key:String):Option[JType] = None

}

object JType {
  def unapply(value:JType) =
    Some(value)
}

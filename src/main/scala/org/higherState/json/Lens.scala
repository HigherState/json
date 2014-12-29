package org.higherState.json

import shapeless.{HNil, HList, ::}

trait Lens[T] {
  def apply(obj:JObject, value:T):JObject
}

case class Setter[T](path:Path, pattern:Pattern[T]) extends Lens[T] {
  def apply(obj:JObject, value:T):JObject =

}

case class Modifier[T](path:Path, pattern:Pattern[T]) extends Lens[T => T]


sealed trait Applicator[L <: HList] {
  type Params <: HList
  def apply(obj:JObject, lenses:L, values:Params):JObject
}

object Applicator {
  implicit val nil = new Applicator[HNil]{
    type Params = HNil
    def apply(obj: JObject, lenses: HNil, values: Params): JObject = obj
  }
  implicit def cons[H, T <: HList](implicit tl: Applicator[T]) =
    new Applicator[Lens[H] :: T]{
      type Params = H :: tl.Params
      def apply(obj:JObject, lenses: Lens[H] :: T, values:Params):JObject = {
        val newObj = lenses.head.apply(obj, values.head)
        tl.apply(newObj, lenses.tail, values.tail)
      }
    }

}

case class Compositor[L <: HList, C](lenses:L)(implicit applicator:Applicator[L]{type Params = C}) {
  //def ~[A](a:A) = Compositor(shapeless.::(a, lenses))
  def apply(obj:JObject, values:C) = applicator(obj, lenses, values)
}

package org.higherState.json

import org.higherState.validation.ValidationFailure
import scala.reflect._

sealed trait DataState
case object New extends DataState
case object Delta extends DataState

trait Validator[V] {

  def validator:PartialFunction[(DataState, Option[V]), List[ValidationFailure]]
}

trait Validate[V] extends Validator[V] {

  def validate(state:DataState, jType:Option[V]):List[ValidationFailure]
}

case class UnexpectedTypeFailure() extends ValidationFailure
case class ImmutableFailure() extends ValidationFailure
case class RequiredFailure() extends ValidationFailure


trait Contract[+T] {

  def unapply(data:Any):Option[T]
}

trait ObjectContract[+T, U] extends Contract[T] with Validate[U] {

  def parent:Parent
  def get(key:String, data:Any):Option[U]
  def validate(state:DataState, jType:Option[U]):List[ValidationFailure] =
    validator.lift(state -> jType).getOrElse(Nil)

  protected var children:List[(String, Validate[U])] = Nil

  protected def property[V <: U:ClassTag](key:String) =
    append(key, PropertyValueContract[T, U, V](key, this, Nil))

  protected def property[W <: Contract[U]{def copy(parent:Parent):W}](contract:W, key:String):W =
    append(key, contract.copy(Some(key -> this)))

  private def append[W <: Validate[U]](key:String, contract:W):W = {
    children = key -> contract :: children
    contract
  }

  def validator:PartialFunction[(DataState, Option[U]), List[ValidationFailure]]
}


trait JObjectContract extends ObjectContract[JObject, JType] {
  //implicit def me = this

  def unapply(data:Any):Option[JObject] =
    parent.fold {
      data match {
        case v: JObject => Some(v)
        case _ => None
      }
    } { p =>
      p._2.get(p._1, data).collect {
        case j:JObject => j
      }
    }

  def get(key:String, data:Any) =
    unapply(data).flatMap(_.value.get(key))

  def validator:PartialFunction[(DataState, Option[JType]), List[ValidationFailure]] = {
    case p@(ds, Some(value:JObject)) =>
      children.flatMap(p => p._2.validate(ds, value.get(p._1)))
    case (_, Some(jType)) if !jType.isInstanceOf[JObject] =>
      List(UnexpectedTypeFailure())
  }
}

case class PropertyValueContract[T, U, V <: U : ClassTag](key:String, parent:ObjectContract[T, U], validators:List[Validator[U]]) extends Contract[V] with Validate[U] {

  private val rc = classTag[V].runtimeClass

  def validate(state:DataState, jType:Option[U]):List[ValidationFailure] =
    validator.lift.apply(state -> jType).getOrElse(Nil) ++ validators.flatMap(_.validator.lift(state -> jType).getOrElse(Nil))


  def being[W >: V](validator:Validator[W]) =
    copy(validators = validators :+ validator)
  def and[W >: V](validator:Validator[W]) =
    being(validator)
  def having[W >: V](validator:Validator[W]) =
    being(validator)

  def unapply(data: Any): Option[V] =
    parent.get(key, data).map {
      case value if rc.isAssignableFrom(value.getClass) =>
        value.asInstanceOf[V] //TODO, use type tag
    }

  def validator:PartialFunction[(DataState, Option[U]), List[ValidationFailure]] = {
    case (_, Some(value)) if !rc.isAssignableFrom(value.getClass) =>
      List(UnexpectedTypeFailure())
  }
}


object Validators {

  val Immutable = new Validator[Any] {
    def validator: PartialFunction[(DataState, Option[JType]), List[ValidationFailure]] = {
      case v@(Delta, Some(_)) =>
        List(ImmutableFailure())
      case v =>
        Nil
    }
  }

  val Required = new Validator[Any] {
    def validator:PartialFunction[(DataState, Option[JType]), List[ValidationFailure]] = {
      case v@(New, None) =>
        List(RequiredFailure())
      case v =>
        Nil
    }
  }
}

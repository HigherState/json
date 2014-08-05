package org.higherState.json

import org.higherState.validation.ValidationFailure
import scala.reflect._

sealed trait DataState
case object New extends DataState
case object Delta extends DataState

trait Validator {

  def validator:PartialFunction[(DataState, Option[Any]), List[ValidationFailure]]
}

trait Validate extends Validator {

  def validate(state:DataState, data:Option[Any]):List[ValidationFailure]
}

case class UnexpectedTypeFailure() extends ValidationFailure
case class ImmutableFailure() extends ValidationFailure
case class RequiredFailure() extends ValidationFailure
case class PlaceHolderFailure() extends ValidationFailure


trait Contract[T] {

  def unapply(data:Any):Option[T]
}

trait ObjectContract[T, U] extends Contract[T] with Validate {

  def parent:Parent
  def get(key:String, data:Any):Option[U]
  def validate(state:DataState, jType:Option[Any]):List[ValidationFailure] =
    validator.lift(state -> jType).getOrElse(Nil)

  protected var children:List[(String, Validate)] = Nil

  protected def property[V <: U:ClassTag](key:String) =
    append(key, PropertyValueContract[T, U, V](key, this, Nil))

  protected def property[W <: Contract[U] with Validate{def copy(parent:Parent):W}](contract:W, key:String):W =
    append(key, contract.copy(Some(key -> this)))

  private def append[W <: Validate](key:String, contract:W):W = {
    children = key -> contract :: children
    contract
  }
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

  def validator:PartialFunction[(DataState, Option[Any]), List[ValidationFailure]] = {
    case p@(ds, Some(value:JObject)) =>
      children.flatMap(p => p._2.validate(ds, value.get(p._1)))
    case (_, Some(jType)) if !jType.isInstanceOf[JObject] =>
      List(UnexpectedTypeFailure())
  }
}

case class PropertyValueContract[T, U, V <: U : ClassTag](key:String, parent:ObjectContract[T, U], validators:List[Validator]) extends Contract[V] with Validate {

  private val rc = classTag[V].runtimeClass

  def validate(state:DataState, data:Option[Any]):List[ValidationFailure] =
    validator.lift.apply(state -> data).getOrElse(Nil) ++ validators.flatMap(_.validator.lift(state -> data).getOrElse(Nil))


  def being(validator:Validator) =
    copy(validators = validators :+ validator)
  def and(validator:Validator) =
    being(validator)
  def having(validator:Validator) =
    being(validator)

  def unapply(data: Any): Option[V] =
    parent.get(key, data).map {
      case value if rc.isAssignableFrom(value.getClass) =>
        value.asInstanceOf[V] //TODO, use type tag
    }

  def validator:PartialFunction[(DataState, Option[Any]), List[ValidationFailure]] = {
    case (_, Some(value)) if !rc.isAssignableFrom(value.getClass) =>
      List(UnexpectedTypeFailure())
  }
}


object Validators {

  val Immutable = new Validator {
    def validator: PartialFunction[(DataState, Option[Any]), List[ValidationFailure]] = {
      case v@(Delta, Some(_)) =>
        List(ImmutableFailure())
      case v =>
        Nil
    }
  }

  val Required = new Validator {
    def validator:PartialFunction[(DataState, Option[Any]), List[ValidationFailure]] = {
      case v@(New, None) =>
        List(RequiredFailure())
      case v =>
        Nil
    }
  }

  def >(value:Double) = new Validator {
    def validator: PartialFunction[(DataState, Option[Any]), List[ValidationFailure]] = {
      case (_, Some(i:Int)) if i <= value =>
        List(PlaceHolderFailure())
      case (_, Some(l:Long)) if l <= value =>
        List(PlaceHolderFailure())
      case (_, Some(d:Double)) if d <= value =>
        List(PlaceHolderFailure())
      case (_, Some(f:Float)) if f <= value =>
        List(PlaceHolderFailure())
      case (_, Some(_)) =>
        List(PlaceHolderFailure())
    }
  }
}

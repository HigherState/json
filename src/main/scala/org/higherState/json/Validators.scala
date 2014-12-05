package org.higherState.json

import org.higherState.validation.ValidationFailure

sealed trait DataState
case object New extends DataState
case object Delta extends DataState

trait Validator {

 def validate(value:Option[JType], currentState:Option[JType], path:Path):Seq[ValidationFailure]

 def && (v:Validator):Validator = AndValidator(this, v)

 def || (v:Validator):Validator = OrValidator(this, v)
}

case class UnexpectedTypeFailure(path:Path, expectedType:String) extends ValidationFailure
case class ImmutableFailure(path:Path) extends ValidationFailure
case class RequiredFailure(path:Path) extends ValidationFailure
case class BoundFailure(path:Path, message:String) extends ValidationFailure
case class NotNullFailure(path:Path) extends ValidationFailure
case class ReservedFailure(path:Path) extends ValidationFailure

case class AndValidator(left:Validator, right:Validator) extends Validator {
  def validate(value:Option[JType], currentState:Option[JType], path:Path):Seq[ValidationFailure] =
    left.validate(value, currentState, path:Path) ++ right.validate(value, currentState, path:Path)
}

case class OrValidator(left:Validator, right:Validator) extends Validator {
  def validate(value:Option[JType], currentState:Option[JType], path:Path):Seq[ValidationFailure] ={
    left.validate(value, currentState, path:Path) match {
      case Seq() => Seq()
      case list => right.validate(value, currentState, path:Path) match {
        case Seq() => Seq()
        case list2 if list2.size < list.size => list2
        case _ => list
      }
    }
  }
}

case object EmptyValidator extends Validator {
  def validate(value: Option[JType], currentState: Option[JType], path: Path): Seq[ValidationFailure] =
    Nil
}

trait SimpleValidator extends Validator {
  def maybeValid(path:Path):PartialFunction[(Option[JType],Option[JType]), ValidationFailure]

  def validate(value: Option[JType], currentState: Option[JType], path:Path): Seq[ValidationFailure] =
    maybeValid(path).lift(value -> currentState).toSeq
}

object Validators {

  val immutable = new SimpleValidator {
    def maybeValid(path:Path) = {
      case (Some(a), Some(b)) if a != b =>
        ImmutableFailure(path)
    }
  }

  val required = new SimpleValidator  {
    def maybeValid(path:Path) = {
      case (None, None) =>
        RequiredFailure(path)
    }
  }

  val reserved = new SimpleValidator  {
    def maybeValid(path:Path) = {
      case (Some(_), _) =>
        ReservedFailure(path)
    }
  }

  val notNull = new SimpleValidator {
    def maybeValid(path: Path) = {
      case (Some(JNull), _) =>
        NotNullFailure(path)
    }
  }

  def >(value:Number) = new SimpleValidator {
    private val double = value.doubleValue()
    def maybeValid(path:Path) = {
      case (Some(JNumber(n)), _) if n.doubleValue() <= double =>
        BoundFailure(path, s"Value $n is not greater than $value")
    }
  }

  def >=(value:Number) = new SimpleValidator {
    private val double = value.doubleValue()
    def maybeValid(path:Path) = {
      case (Some(JNumber(n)), _) if n.doubleValue() < double =>
        BoundFailure(path, s"Value $n is not greater than or equal to $value")
    }
  }

  def <(value:Number) = new SimpleValidator {
    private val double = value.doubleValue()
    def maybeValid(path:Path) = {
      case (Some(JNumber(n)), _) if n.doubleValue() >= double =>
        BoundFailure(path, s"Value $n is not less than $value")
    }
  }

  def <=(value:Number) = new SimpleValidator {
    private val double = value.doubleValue()
    def maybeValid(path:Path) = {
      case (Some(JNumber(n)), _) if n.doubleValue() >= double =>
        BoundFailure(path, s"Value $n is not less than or equal to $value")
    }
  }

  def as[T <: Contract](contract:T) = new Validator {
    def validate(value: Option[JType], currentState: Option[JType], path: Path): Seq[ValidationFailure] =
      value.collect {
        case JArray(seq) =>
          seq.zipWithIndex.flatMap{
            case (j:JObject, i) =>
              contract.validate(j, None, path \ i.toString)
            case (_, i) =>
              Seq(UnexpectedTypeFailure(path \ i.toString, "JObject"))
          }
      }.getOrElse(Seq.empty)
  }
}

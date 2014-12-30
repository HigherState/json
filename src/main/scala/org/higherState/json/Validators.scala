package org.higherState.json

import org.higherState.validation.ValidationFailure

sealed trait DataState
case object New extends DataState
case object Delta extends DataState

trait Validator {

  def validate(value:Option[JType], currentState:Option[JType], path:Path):Seq[ValidationFailure]

  def && (v:Validator):Validator = AndValidator(this, v)

  def || (v:Validator):Validator = OrValidator(this, v)

  def getSchema:JObject
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

  def getSchema: JObject =
    left.getSchema ++ right.getSchema
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
  def getSchema:JObject =
    JObject("or" -> JArray(left.getSchema, right.getSchema))
}

case object EmptyValidator extends Validator {
  def validate(value: Option[JType], currentState: Option[JType], path: Path): Seq[ValidationFailure] =
    Nil

  def getSchema: JObject = JObject.empty
}

trait SimpleValidator extends Validator {
  def maybeValid(path:Path):PartialFunction[(Option[JType],Option[JType]), ValidationFailure]

  def validate(value: Option[JType], currentState: Option[JType], path:Path): Seq[ValidationFailure] =
    maybeValid(path).lift(value -> currentState).toSeq
}

object Validation {

  val immutable = new SimpleValidator {
    def maybeValid(path:Path) = {
      case (Some(a), Some(b)) if a != b =>
        ImmutableFailure(path)
    }

    def getSchema: JObject = JObject("immutable" -> JTrue)
  }

  val required = new SimpleValidator  {
    def maybeValid(path:Path) = {
      case (None, None) =>
        RequiredFailure(path)
    }
    def getSchema: JObject = JObject("required" -> JTrue)
  }

  val notNull = new SimpleValidator {
    def maybeValid(path: Path) = {
      case (Some(JNull), _) =>
        NotNullFailure(path)
    }
    def getSchema: JObject = JObject("notNull" -> JTrue)
  }

  val reserved = new SimpleValidator  {
    def maybeValid(path:Path) = {
      case (Some(_), _) =>
        ReservedFailure(path)
    }
    def getSchema: JObject = JObject("reserved" -> JTrue)
  }

  def >(value:Number) = new SimpleValidator {
    private val double = value.doubleValue()
    def maybeValid(path:Path) = {
      case (Some(JNumber(n)), _) if n.doubleValue() <= double =>
        BoundFailure(path, s"Value $n is not greater than $value")
    }
    def getSchema: JObject = JObject("greaterThan" -> double.j)
  }

  def >=(value:Number) = new SimpleValidator {
    private val double = value.doubleValue()
    def maybeValid(path:Path) = {
      case (Some(JNumber(n)), _) if n.doubleValue() < double =>
        BoundFailure(path, s"Value $n is not greater than or equal to $value")
    }
    def getSchema: JObject = JObject("greaterThanEquals" -> double.j)
  }

  def <(value:Number) = new SimpleValidator {
    private val double = value.doubleValue()
    def maybeValid(path:Path) = {
      case (Some(JNumber(n)), _) if n.doubleValue() >= double =>
        BoundFailure(path, s"Value $n is not less than $value")
    }
    def getSchema: JObject = JObject("lessThan" -> double.j)
  }

  def <=(value:Number) = new SimpleValidator {
    private val double = value.doubleValue()
    def maybeValid(path:Path) = {
      case (Some(JNumber(n)), _) if n.doubleValue() >= double =>
        BoundFailure(path, s"Value $n is not less than or equal to $value")
    }
    def getSchema: JObject = JObject("lessThanEquals" -> double.j)
  }

  def minLength(value:Int) = new SimpleValidator {
    def maybeValid(path: Path) = {
      case (Some(JArray(seq)), _) if seq.length < value =>
        BoundFailure(path, s"Array must have length of at least $value")
    }
    def getSchema: JObject = JObject("minLength" -> value.j)
  }

  def nonEmpty = new SimpleValidator {
    def maybeValid(path: Path) = {
      case (Some(JArray(seq)), _) if seq.isEmpty =>
        BoundFailure(path, s"Array must not be empty")
    }

    def getSchema: JObject = JObject("nonEmpty" -> JTrue)
  }
  def forall(validator:Validator) = new Validator {
    def validate(value: Option[JType], currentState: Option[JType], pathContext: Path): Seq[ValidationFailure] =
      value collect {
        case JArray(seq) =>
          for {
            (e,i) <- seq.zipWithIndex
            v <- validator.validate(Some(e), None, pathContext \ i)
          } yield v
      } getOrElse Seq.empty

    def getSchema = JObject("items" -> validator.getSchema)
  }
}
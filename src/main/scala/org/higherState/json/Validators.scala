package org.higherState.json

import scalaz.{NonEmptyList, Failure, Success}

trait Validator[+T] {

  def validate(value:Option[Json], currentState:Option[Json], path:Path):Seq[(String, Path)]

  def &&[S >: T] (v:Validator[S]):Validator[S] = AndValidator(this, v)

  def ||[S >: T] (v:Validator[S]):Validator[S] = OrValidator(this, v)

  def schema:JObject
}

case class AndValidator[T, A >: T, B >: T](left:Validator[A], right:Validator[B]) extends Validator[T] {
  def validate(value:Option[Json], currentState:Option[Json], path:Path):Seq[(String, Path)] =
    left.validate(value, currentState, path:Path) ++ right.validate(value, currentState, path:Path)

  def schema: JObject =
    JObject(left.schema.value ++ right.schema.value)
}

case class OrValidator[T, A >: T, B >: T](left:Validator[A], right:Validator[B]) extends Validator[T] {
  def validate(value:Option[Json], currentState:Option[Json], path:Path):Seq[(String, Path)] ={
    left.validate(value, currentState, path:Path) match {
      case Seq() => Seq()
      case list => right.validate(value, currentState, path:Path) match {
        case Seq() => Seq()
        case list2 if list2.size < list.size => list2
        case _ => list
      }
    }
  }
  def schema:JObject =
    JObject("or" -> JArray(left.schema, right.schema))
}

case object EmptyValidator extends Validator[Nothing] {
  def validate(value: Option[Json], currentState: Option[Json], path: Path): Seq[(String, Path)] =
    Nil

  def schema: JObject = JObject.empty
}

trait SimpleValidator[+T] extends Validator[T] {
  def maybeValid(path:Path):PartialFunction[(Option[Json],Option[Json]), (String, Path)]

  def validate(value: Option[Json], currentState: Option[Json], path:Path): Seq[(String, Path)] =
    maybeValid(path).lift(value -> currentState).toSeq
}

object JsonValidation {
  import JsonConstructor._

  type Numeric = Long with Int with Float with Double with Option[Long] with Option[Int] with Option[Float] with Option[Double]
  type Length = String with Seq[Nothing]
  type Optionable[T] = T with Option[T]

  implicit class BaseContractValidation(val contract:BaseContract) extends AnyVal {
    def validate(newContent:Json):JValid =
      validate(newContent, None, Path.empty) match {
        case Nil => Success(newContent)
        case failure +: failures => Failure(NonEmptyList(failure, failures:_*))
      }

    def validate(deltaContent:Json, currentState:Json):JValid =
      validate(deltaContent, Some(currentState), Path.empty) match {
        case Nil => Success(deltaContent)
        case failure +: failures => Failure(NonEmptyList(failure, failures:_*))
      }
    //TODO better approach here
    def validate(value: Json, currentState: Option[Json], path:Path): Seq[(String, Path)] =
      contract.contractProperties.flatMap{p =>
        val v = JsonPath.getValue(value, p.relativePath.segments)
        val c = currentState.flatMap(JsonPath.getValue(_, p.relativePath.segments))
        p.validate(v, c, path ++ p.relativePath)
      }
  }

  implicit class PropertyValidation[T](val prop:Property[T]) extends AnyVal {
    def validate(value: Option[Json], currentState: Option[Json], path:Path): Seq[(String, Path)] =
      ((value, currentState, prop) match {
        case (None, None, p: Expected[_]) =>
          Seq("Value required." -> path)
        case (Some(v), c, _) if prop.pattern.unapply(v).isEmpty =>
          Seq(s"Unexpected type '${v.getClass.getSimpleName}'." -> path)
        case (Some(v), c, b:BaseContract) =>
          new BaseContractValidation(b).validate(v, c, path)
        case _ =>
          Seq.empty
      }) ++ prop.validator.validate(value, currentState, path)
  }

  val immutable = new SimpleValidator[Nothing] {
    def maybeValid(path:Path) = {
      case (Some(a), Some(b)) if a != b =>
        "value is immutable and cannot be changed." -> path
    }
    def schema: JObject = JObject("immutable" -> JTrue)
  }

  val notNull = new SimpleValidator[Option[Nothing]] {
    def maybeValid(path: Path) = {
      case (Some(JNull), _) =>
        "Value cannot be null." -> path
    }

    def schema: JObject = JObject("notNull" -> JTrue)
  }

  val reserved = new SimpleValidator[Option[Nothing]]  {
    def maybeValid(path:Path) = {
      case (Some(_), _) =>
        "Value is reserved and cannot be provided." -> path
    }
    def schema: JObject = JObject("reserved" -> JTrue)
  }

  sealed trait BoundedValidator extends SimpleValidator[Numeric] {
    def doubleFail(n: Double): Boolean

    def longFail(n: Long): Boolean

    def message(n: Number): String

    def maybeValid(path: Path) = {
      case (Some(JDouble(n)), _) if doubleFail(n) =>
        message(n) -> path
      case (Some(JLong(n)), _) if longFail(n) =>
        message(n) -> path
    }
  }

  def >[T](value: Long) = new BoundedValidator {
    def doubleFail(n: Double): Boolean = n <= value

    def longFail(n: Long): Boolean = n <= value

    def message(n: Number): String = s"Value $n is not greater than $value"

    def schema: JObject = JObject("greaterThan" -> value.j)
  }

  def >[T](value: Double):Validator[Numeric] = new BoundedValidator {
    def doubleFail(n: Double): Boolean = n <= value

    def longFail(n: Long): Boolean = n <= value

    def message(n: Number): String = s"Value $n is not greater than $value"

    def schema: JObject = JObject("greaterThan" -> value.j)
  }

  def >=[T](value: Long):Validator[Numeric] = new BoundedValidator {
    def doubleFail(n: Double): Boolean = n < value

    def longFail(n: Long): Boolean = n < value

    def message(n: Number): String = s"Value $n is not greater than or equal to $value"

    def schema: JObject = JObject("greaterThanEquals" -> value.j)
  }

  def >=[T](value: Double) = new BoundedValidator {
    def doubleFail(n: Double): Boolean = n < value

    def longFail(n: Long): Boolean = n < value

    def message(n: Number): String = s"Value $n is not greater than or equal to $value"

    def schema: JObject = JObject("greaterThanEquals" -> value.j)
  }

  def <[T](value: Long) = new BoundedValidator {
    def doubleFail(n: Double): Boolean = n >= value

    def longFail(n: Long): Boolean = n >= value

    def message(n: Number): String = s"Value $n is not less than $value"

    def schema: JObject = JObject("lessThan" -> value.j)
  }

  def <[T](value: Double) = new BoundedValidator {
    def doubleFail(n: Double): Boolean = n <= value

    def longFail(n: Long): Boolean = n <= value

    def message(n: Number): String = s"Value $n is not less than $value"

    def schema: JObject = JObject("lessThan" -> value.j)
  }

  def <=[T](value: Long) = new BoundedValidator {
    def doubleFail(n: Double): Boolean = n > value

    def longFail(n: Long): Boolean = n > value

    def message(n: Number): String = s"Value $n is not less than or equal to $value"

    def schema: JObject = JObject("lessThanEquals" -> value.j)
  }

  def <=[T](value: Double) = new BoundedValidator {
    def doubleFail(n: Double): Boolean = n > value

    def longFail(n: Long): Boolean = n > value

    def message(n: Number): String = s"Value $n is not greater than or equal to $value"

    def schema: JObject = JObject("lessThanEquals" -> value.j)
  }

  def in[T](values:T*)(implicit pattern:Pattern[T]) = new SimpleValidator[Optionable[T]] {
    def schema: JObject = JObject("isIn" -> JArray(values.map(pattern.apply)))

    def maybeValid(path: Path): PartialFunction[(Option[Json], Option[Json]), (String, Path)] = {
      case (Some(pattern(j)), _) if !values.contains(j) =>
        s"Unexpected type '${j.getClass.getSimpleName}'." -> path
    }
  }

  def minLength(value: Int) = new SimpleValidator[Optionable[Length]] {
    def maybeValid(path: Path) = {
      case (Some(JArray(seq)), _) if seq.length < value =>
        s"Array must have length of at least $value" -> path
    }

    def schema: JObject = JObject("minLength" -> value.j)
  }

  val nonEmpty = new SimpleValidator[Optionable[Length]] {
    def maybeValid(path: Path) = {
      case (Some(JArray(seq)), _) if seq.isEmpty =>
        s"Array must not be empty" -> path
    }

    def schema: JObject = JObject("nonEmpty" -> JTrue)
  }

  val nonEmptyOrWhiteSpace:Validator[String] = new SimpleValidator[Length] {
    def maybeValid(path: Path) = {
      case (Some(JString(text)), _) if text.trim().isEmpty =>
        s"Text must not be all empty or whitespace" -> path
    }

    def schema: JObject = JObject("nonEmptyOrWhitespace" -> JTrue)
  }

  def forall[T](validator: Validator[T]) = new Validator[Optionable[Seq[Nothing]]] {
    def validate(value: Option[Json], currentState: Option[Json], pathContext: Path): Seq[(String, Path)] =
      value collect {
        case JArray(seq) =>
          for {
            (e, i) <- seq.zipWithIndex
            v <- validator.validate(Some(e), None, pathContext \ i)
          } yield v
      } getOrElse Seq.empty

    def schema = JObject("items" -> validator.schema)
  }

  //TODO Forall doesnt validate agains current state, bit of an odd one..
  def forall(contract: BaseContract) = new Validator[Optionable[Seq[Nothing]]] {
    def validate(value: Option[Json], currentState: Option[Json], pathContext: Path): Seq[(String, Path)] =
      value collect {
        case JArray(seq) =>
          for {
            (e, i) <- seq.zipWithIndex
            v <- BaseContractValidation(contract).validate(e, None, pathContext \ i)
          } yield v
      } getOrElse Seq.empty

    def schema = JObject.empty //("items" -> contract.schema)
  }
}
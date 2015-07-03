package org.higherState.json

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

case object Internal extends SimpleValidator[Option[Nothing]]  {
  def maybeValid(path:Path) = {
    case (Some(_), _) =>
      "Value is reserved and cannot be provided." -> path
  }
  def schema: JObject = JObject("reserved" -> JTrue)
}

trait SimpleValidator[+T] extends Validator[T] {
  def maybeValid(path:Path):PartialFunction[(Option[Json],Option[Json]), (String, Path)]

  def validate(value: Option[Json], currentState: Option[Json], path:Path): Seq[(String, Path)] =
    maybeValid(path).lift(value -> currentState).toSeq
}


trait ValidationPropertyCache {
  private var properties:Map[Class[_], Seq[Property[_]]] = Map.empty
  private var internal:Map[Class[_], Seq[Property[_]]] = Map.empty
  def getProperties(contract:BaseContract):Seq[Property[_]] =
    properties.get(contract.getClass) match {
      case Some(p) =>
        p
      case None =>
        val vp =
          contract.getClass.getMethods
            .filter(m => m.getParameterTypes.isEmpty && classOf[Property[_]].isAssignableFrom(m.getReturnType))
            .map(_.invoke(contract).asInstanceOf[Property[_]])
            .toSeq
        this.synchronized {
          properties = properties + (contract.getClass -> vp)
        }
        vp
    }

  def getInternal(contract:BaseContract):Seq[Property[_]] = {
    internal.get(contract.getClass) match {
      case Some(p) =>
        p
      case None =>
        val ip = getProperties(contract).collect {
          case p if isInternal(p.validator) => Seq(p)
          case p: BaseContract =>
            getInternal(p)
        }.flatten
        this.synchronized {
          internal = internal + (contract.getClass -> ip)
        }
        ip
    }
  }

  private val isInternal:Function[Validator[_], Boolean] = {
    case AndValidator(l,r) =>
      isInternal(l) || isInternal(r)
    case OrValidator(l, r) => // bit odd through excpetion for now
      throw new Exception("Internal json validator shouldn't be in an 'or' conditional")
    case v => v == Internal
  }
}

trait JsonValidators {
  import DefaultPatterns._
  import JsonConstructor._
  import JsonPath._


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

  val internal = Internal

  sealed trait BoundedValidator extends SimpleValidator[JNumeric] {
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

  def >[T](value: Double):Validator[JNumeric] = new BoundedValidator {
    def doubleFail(n: Double): Boolean = n <= value

    def longFail(n: Long): Boolean = n <= value

    def message(n: Number): String = s"Value $n is not greater than $value"

    def schema: JObject = JObject("greaterThan" -> value.j)
  }

  def >=[T](value: Long):Validator[JNumeric] = new BoundedValidator {
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

  def in[T](values:T*)(implicit pattern:Pattern[T]) = new SimpleValidator[JOptionable[T]] {
    def schema: JObject = JObject("is in" -> JArray(values.map(pattern.apply)))

    def maybeValid(path: Path): PartialFunction[(Option[Json], Option[Json]), (String, Path)] = {
      case (Some(pattern(j)), _) if !values.contains(j) =>
        "Value outside of allowed values." -> path
    }
  }

  def nin[T](values:T*)(implicit pattern:Pattern[T]) = new SimpleValidator[JOptionable[T]] {
    def schema: JObject = JObject("notIn" -> JArray(values.map(pattern.apply)))

    def maybeValid(path: Path): PartialFunction[(Option[Json], Option[Json]), (String, Path)] = {
      case (Some(pattern(j)), _) if !values.contains(j) =>
        "Value not allowed." -> path
    }
  }

  def inCaseInsensitive(values:String*) = new SimpleValidator[JOptionable[String]] {
    def schema: JObject = JObject("is in" -> JArray(values.map(JString)), "caseInsensitive" -> JTrue)

    def maybeValid(path: Path): PartialFunction[(Option[Json], Option[Json]), (String, Path)] = {
      case (Some(JString(s)), _) if !values.exists(_.equalsIgnoreCase(s)) =>
        "Value outside of allowed values." -> path
    }
  }

  def ninCaseInsensitive(values:String*) = new SimpleValidator[JOptionable[String]] {
    def schema: JObject = JObject("not in" -> JArray(values.map(JString)), "caseInsensitive" -> JTrue)

    def maybeValid(path: Path): PartialFunction[(Option[Json], Option[Json]), (String, Path)] = {
      case (Some(JString(s)), _) if values.exists(_.equalsIgnoreCase(s)) =>
        "Value outside of allowed values." -> path
    }
  }

  def minLength(value: Int) = new SimpleValidator[JOptionable[JLength]] {
    def maybeValid(path: Path) = {
      case (Some(JArray(seq)), _) if seq.length < value =>
        s"Array must have length of at least $value" -> path
      case (Some(JString(s)), _) if s.length < value =>
        s"String must have length of at least $value" -> path
    }

    def schema: JObject = JObject("minLength" -> value.j)
  }

  def maxLength(value: Int) = new SimpleValidator[JOptionable[JLength]] {
    def maybeValid(path: Path) = {
      case (Some(JArray(seq)), _) if seq.length > value =>
        s"Array must have length of no greater than $value" -> path
      case (Some(JString(s)), _) if s.length > value =>
        s"String must have length of no greater than $value" -> path
    }

    def schema: JObject = JObject("maxLength" -> value.j)
  }

  val nonEmpty = new SimpleValidator[JOptionable[JLength]] {
    def maybeValid(path: Path) = {
      case (Some(JArray(seq)), _) if seq.isEmpty =>
        s"Array must not be empty" -> path
      case (Some(JString(s)), _) if s.isEmpty =>
        s"String must not be empty" -> path
    }

    def schema: JObject = JObject("nonEmpty" -> JTrue)
  }

  val nonEmptyOrWhiteSpace:Validator[String] = new SimpleValidator[JLength] {
    def maybeValid(path: Path) = {
      case (Some(JString(text)), _) if text.trim().isEmpty =>
        s"Text must not be all empty or whitespace" -> path
    }

    def schema: JObject = JObject("nonEmptyOrWhitespace" -> JTrue)
  }

  def forall[T](validator: Validator[T]) = new Validator[JOptionable[Seq[Nothing]]] {
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
  def forall(contract: BaseContract) = new Validator[JOptionable[Seq[Nothing]]] {
    def validate(value: Option[Json], currentState: Option[Json], pathContext: Path): Seq[(String, Path)] =
      value collect {
        case JArray(seq) =>
          for {
            (e, i) <- seq.zipWithIndex
            v <- JsonValidation.BaseContractValidation(contract).$validate(e, None, pathContext \ i)
          } yield v
      } getOrElse Seq.empty

    def schema = JObject.empty //("items" -> contract.schema)
  }

  def values(contract:BaseContract) = new Validator[JOptionable[Map[String,Nothing]]] {
    def validate(value: Option[Json], currentState: Option[Json], path: Path): Seq[(String, Path)] =
      value collect {
        case JObject(map) =>
          map.flatMap{ kv =>
            val current = currentState \ kv._1
            JsonValidation.BaseContractValidation(contract).$validate(kv._2, current, path \ kv._1)
          }.toSeq
      } getOrElse Seq.empty[(String, Path)]

    def schema: JObject = ???
  }
}

object DefaultValidators extends JsonValidators


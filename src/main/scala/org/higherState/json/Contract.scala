package org.higherState.json

import reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror=>cm,universe=>ru}
import org.higherState.validation.ValidationFailure
import JsonConstructor._
import JsonPath._

trait Contract extends PropertyMapper with Validator {
  implicit protected def path:Path = Path.empty

  def validate(value:Json, currentState:Option[Json] = None):Seq[ValidationFailure] =
    contractProperties.flatMap(p => p.validate(value \ p.key, currentState \ p.key, Path.empty \ p.key))

  def apply[R](f:this.type => R):R = f(this)

  def validate(value: Option[Json], currentState: Option[Json], path: Path): Seq[ValidationFailure] =
    value.fold(Seq.empty[ValidationFailure]){ v =>
      contractProperties.flatMap(p => p.validate(v \ p.key, currentState \ p.key, path \ p.key))
    }

  def schema = JObject(
    "type" -> "object".j,
    "properties" -> JObject(contractProperties.map(p => p.key -> p.schema):_*)
  )
}

trait SubContract extends PropertyMapper {
  implicit protected def path:Path
}

sealed trait Property extends Validator{
  def key:String
}
sealed trait PropertyMapper {
  lazy val contractProperties:Seq[Property] = {
    val r = cm.reflect(this)
    val t = ru.appliedType(r.symbol.asType.toType)
    val propertyErasure = typeOf[Property].erasure
    t.members.collect{ case m:MethodSymbol if m.isPublic && m.returnType <:< propertyErasure  && !m.isConstructor =>
      r.reflectMethod(m)().asInstanceOf[Property]// reflectField doesnt work oddly
    }.toSeq
  }

  def schema:JObject
}

sealed trait ValueLens[T] {
  def path:Path
  protected def pattern:Pattern[T]

  def unapply(t:Json):Option[T] =
    get(t)

  def get(t:Json):Option[T] =
    Lens.getValue(t, path.segments).flatMap(pattern.unapply)
  def set =
    (value:T) => (j:Json) => Lens.setValue(Some(j), path.segments, pattern(value))
  def modify =
    (func:T => T) => (j:Json) =>
      Lens.getValue(j, path.segments)
        .flatMap(pattern.unapply)
        .fold[Json](j)(v => Lens.setValue(Some(j), path.segments, pattern(func(v))))
  def maybeModify =
    (func:Option[T] => T) => (j:Json) => {
      val current = Lens.getValue(j, path.segments).flatMap(pattern.unapply)
      Lens.setValue(Some(j), path.segments, pattern(func(current)))
    }
  def drop =
    (j:Json) => Lens.dropValue(j, path.segments)
  def move =
    (p:ValueLens[T]) => (j:Json) => {
      Lens.getValue(j, path.segments) match {
        case None =>
          Lens.dropValue(j, p.path.segments)
        case Some(value) =>
          val j2 = Lens.dropValue(j, path.segments)
          Lens.insertValue(Some(j2), p.path.segments, value)
      }
    }
}

abstract class Object(val key:String, validator:Validator = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[JObject])
  extends PropertyMapper with Property {
  implicit protected val path:Path = parentPath \ key

  def apply[R](f:this.type => R):R = f(this)

  def validate(value: Option[Json], currentState: Option[Json], path:Path): Seq[ValidationFailure] = {
    val patternValidate = pattern.validate(value, currentState, path)
    val valid = validator.validate(value, currentState, path)
    val properties =
      value.fold(Seq.empty[ValidationFailure])(v => contractProperties.flatMap(p => p.validate(v \ p.key, currentState \ p.key, path \ p.key)))
    patternValidate ++ valid ++ properties
  }

  def schema:JObject =
    JObject {
      if (contractProperties.isEmpty)
        pattern.schema.value ++ validator.schema.value
      else
        pattern.schema.value ++ validator.schema.value + ("properties" -> JObject(contractProperties.map(p => p.key -> p.schema): _*))
    }
}

case class Value[T <: Any](key:String, validator:Validator = EmptyValidator)
                          (implicit parentPath:Path, protected val pattern:Pattern[T])
                          extends Property with ValueLens[T] {
  val path:Path = parentPath \ key

  def validate(value: Option[Json], currentState: Option[Json], path:Path): Seq[ValidationFailure] = {
    val patternValidate = pattern.validate(value, currentState, path)
    val valid = validator.validate(value, currentState, path)
    patternValidate ++ valid
  }

  val ? = Maybe(unapply)

  def schema:JObject =
    JObject(pattern.schema.value ++ validator.schema.value)
}

case class Array[T](key:String, validator:Validator = EmptyValidator)
                   (implicit parentPath:Path, protected val pattern:Pattern[Seq[T]], elementPattern:Pattern[T])
  extends Property with ValueLens[Seq[T]] {

  val path:Path = parentPath \ key

  def apply[R](f:this.type => R):R = f(this)

  def at(index:Int) = ArrayElement(index, this)

  def head = ArrayElement(0, this)

  def validate(value: Option[Json], currentState: Option[Json], path:Path): Seq[ValidationFailure] = {
    val patternValidate = pattern.validate(value, currentState, path)
    val valid = validator.validate(value, currentState, path)
    patternValidate ++ valid
  }

  val ? = Maybe(unapply)

  def append =
    (value:T) => modify(s => s :+ value)

  def schema:JObject =
    JObject(pattern.schema.value ++ validator.schema.value)
}

case class ArrayElement[T](index:Int, a:Array[T])(implicit protected val pattern:Pattern[T]) extends ValueLens[T]  {
  def path = a.path \ index
}

case class Maybe[T](f:Json => Option[T]) {
  def unapply(t:Json) =
    Some(f(t))
}




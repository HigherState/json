package org.higherState.json

import reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror=>cm,universe=>ru}
import org.higherState.validation.ValidationFailure
import JsonConstructor._
import JsonPath._

abstract class Extractor extends PropertyMapper with Validator {
  implicit protected def path:Path = Path.empty

  def validate(value:Json, currentState:Option[Json] = None):Seq[ValidationFailure] =
    contractProperties.flatMap(p => p.validate(value \ p.key, currentState \ p.key, Path.empty))

  def apply[R](f:this.type => R):R = f(this)

  def validate(value: Option[Json], currentState: Option[Json], path: Path): Seq[ValidationFailure] =
    value.fold(Seq.empty[ValidationFailure]){ v =>
      contractProperties.flatMap(p => p.validate(v \ p.key, currentState \ p.key, path))
    }

  def getSchema = JObject(
    "type" -> "object".j,
    "properties" -> JObject(contractProperties.map(p => p.key -> p.getSchema):_*)
  )
}

trait Contract extends PropertyMapper {
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
    t.members.collect{ case m:MethodSymbol if m.isPublic && m.returnType.erasure == propertyErasure && !m.isConstructor =>
      r.reflectMethod(m)().asInstanceOf[Property]// reflectField doesnt work oddly
    }.toSeq
  }

  def getSchema:JObject
}

sealed trait ValueLens[T] {
  def path:Path
  protected def pattern:Pattern[T]

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
  def clear =
    (j:Json) => Lens.removeValue(j, path.segments)
  def move =
    (p:Value[T]) => (j:Json) => {
      Lens.getValue(j, path.segments) match {
        case None =>
          Lens.removeValue(j, p.path.segments)
        case Some(value) =>
          val j2 = Lens.removeValue(j, path.segments)
          Lens.setValue(Some(j2), p.path.segments, value)
      }
    }
}

abstract class Object(val key:String, validator:Validator = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[JObject])
  extends PropertyMapper with Property {
  implicit protected val path:Path = parentPath \ key

  def apply[R](f:this.type => R):R = f(this)

  def validate(value: Option[Json], currentState: Option[Json], path:Path): Seq[ValidationFailure] = {
    val typeMatch =
      value.collect {
        case j if j != JNull && !j.isInstanceOf[JObject] =>
          UnexpectedTypeFailure(path, pattern.toString)
      }

    val valid = validator.validate(value, currentState, path)
    val properties = contractProperties.flatMap(p => p.validate(value \ p.key, currentState \ p.key, path \ p.key))
    valid ++ typeMatch ++ properties
  }

  def getSchema:JObject =
    JObject {
      if (contractProperties.isEmpty)
        pattern.getSchema.value ++ validator.getSchema.value
      else
        pattern.getSchema.value ++ validator.getSchema.value + ("properties" -> JObject(contractProperties.map(p => p.key -> p.getSchema): _*))
    }
}

case class Value[T <: Any](key:String, validator:Validator = EmptyValidator)
                          (implicit parentPath:Path, protected val pattern:Pattern[T])
                          extends Property with ValueLens[T] {
  val path:Path = parentPath \ key

  def unapply(t:Json):Option[T] =
    get(t)

  def validate(value: Option[Json], currentState: Option[Json], path:Path): Seq[ValidationFailure] = {
    val typeMatch =
      value.collect {
        case j if j != JNull && pattern.unapply(j).isEmpty =>
          UnexpectedTypeFailure(path, pattern.toString)
      }

    val valid = validator.validate(value, currentState, path)
    valid ++ typeMatch
  }

  val ? = Maybe(unapply)

  def getSchema:JObject =
    JObject(pattern.getSchema.value ++ validator.getSchema.value)
}

//case class ObjectArray[T <: Contract](key:String, validator:Validator = EmptyValidator)
//                                     (implicit parentPath:Path, protected val pattern:Pattern[Seq[Json]])
//  extends Property with ValueLens[Seq[Json]] {
//  def validate(value: Option[Json], currentState: Option[Json], path:Path): Seq[ValidationFailure] = {
//    val typeMatch =
//      value.collect {
//        case j if j != JNull && pattern.unapply(j).isEmpty =>
//          UnexpectedTypeFailure(path, pattern.toString)
//      }
//
//    val valid = validator.validate(value, currentState, path)
//    valid ++ typeMatch
//  }
//}

case class Array[T](key:String, validator:Validator = EmptyValidator)
                   (implicit parentPath:Path, protected val pattern:Pattern[Seq[T]], elementPattern:Pattern[T])
  extends Property with ValueLens[Seq[T]] {

  val path:Path = parentPath \ key

  def apply[R](f:this.type => R):R = f(this)

  def unapply(t:Json):Option[Seq[T]] =
    get(t)

  def at(index:Int) = ArrayElement(index, this)

  def head = ArrayElement(0, this)

  def validate(value: Option[Json], currentState: Option[Json], path:Path): Seq[ValidationFailure] = {
    val typeMatch =
      value.collect {
        case j if j != JNull && pattern.unapply(j).isEmpty =>
          UnexpectedTypeFailure(path, pattern.toString)
      }

    val valid = validator.validate(value, currentState, path)
    valid ++ typeMatch
  }

  val ? = Maybe(unapply)

  def append =
    (value:T) => modify(s => s :+ value)

  def getSchema:JObject =
    JObject(pattern.getSchema.value ++ validator.getSchema.value)
}

case class ArrayElement[T](index:Int, a:Array[T])(implicit pattern:Pattern[T]) {
  def path = a.path \ index
  def unapply(t:Json):Option[T] =
    a.unapply(t).flatMap{ s =>
      if (index < s.length) Some(s(index))
      else None
    }

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
}

case class Maybe[T](f:Json => Option[T]) {
  def unapply(t:Json) =
    Some(f(t))
}




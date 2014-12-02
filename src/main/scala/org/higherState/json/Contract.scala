package org.higherState.json

import scala.reflect._
import reflect.runtime.universe._
import scala.reflect.runtime.{universe=>ru}
import org.higherState.validation.ValidationFailure

abstract class Contract extends PropertyMapper {
  implicit protected def path:Path = Path.empty

  def validate(value:JType, currentState:Option[JType] = None):Seq[ValidationFailure] =
    contractProperties.flatMap(p => p.validate(value \ p.key, currentState \ p.key))
}

trait SubContract extends PropertyMapper {
  implicit protected def path:Path
}

sealed trait PropertyMapper {
  lazy val contractProperties:Seq[Property[_]] = {
    val mirror = ru.runtimeMirror(this.getClass.getClassLoader)
    val r = mirror.reflect(this)
    val t = ru.appliedType(r.symbol.asType)
    val propertyErasure = typeOf[Property[_]].erasure
    t.members.collect{ case m:MethodSymbol if m.isPublic && m.typeSignature.resultType.erasure == propertyErasure =>
      r.reflectMethod(m)().asInstanceOf[Property[_]]// reflectField doesnt work oddly
    }.toSeq
  }
}

class Property[T](val key:String, validator:Validator = EmptyValidator)(implicit parentPath:Path, tag:ClassTag[T]) extends PropertyMapper {
  implicit protected val path:Path = parentPath \ key

  private lazy val _class:Class[_] = {
    val c = classTag[T].runtimeClass
    if (c == classTag[Nothing].runtimeClass) classTag[JType].runtimeClass
    else c
  }

  def unapply(t:JType) = path(t).collect {
    case j if _class.isAssignableFrom(j.getClass) => j.asInstanceOf[T]
    case JType(value) if value != null && _class.isAssignableFrom(value.getClass) => value.asInstanceOf[T]
  }

  val ? = Maybe(unapply)

  def validate(value: Option[JType], currentState: Option[JType]): Seq[ValidationFailure] = {
    val typeMatch =
      value.collect {
        case j if !(j == JNull || _class.isAssignableFrom(j.getClass) || _class.isAssignableFrom(j.value.getClass)) =>
          UnexpectedTypeFailure(path, _class)
      }
    val valid =  validator.validate(value, currentState, path)
    val properties = contractProperties.flatMap(p => p.validate(value \ p.key, currentState \ p.key))
    valid ++ typeMatch ++ properties
  }
}

case class Maybe[T](f:JType => Option[T]) {
  def unapply(t:JType) =
    Some(f(t))
}
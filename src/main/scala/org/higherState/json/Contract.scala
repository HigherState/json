package org.higherState.json

import reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror=>cm,universe=>ru}
import org.higherState.validation.ValidationFailure

abstract class Contract extends PropertyMapper {
  implicit protected def path:Path = Path.empty

  def validate(value:JType, currentState:Option[JType] = None, path:Path = this.path):Seq[ValidationFailure] =
    contractProperties.flatMap(p => p.validate(value \ p.key, currentState \ p.key, path \ p.key))

  def apply[R](f:this.type => R):R = f(this)
}

trait SubContract extends PropertyMapper {
  implicit protected def path:Path
}

sealed trait PropertyMapper {
  lazy val contractProperties:Seq[Property[_]] = {
    val r = cm.reflect(this)
    val t = ru.appliedType(r.symbol.asType.toType)
    val propertyErasure = typeOf[Property[_]].erasure
    t.members.collect{ case m:MethodSymbol if m.isPublic && m.returnType.erasure == propertyErasure && !m.isConstructor =>
      r.reflectMethod(m)().asInstanceOf[Property[_]]// reflectField doesnt work oddly
    }.toSeq
  }
}

class Property[T <: Any](val key:String, validator:Validator = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[T]) extends PropertyMapper {
  implicit protected val path:Path = parentPath \ key

  def unapply(t:JType):Option[T] =
    get(t)

  def apply[R](f:this.type => R):R = f(this)

  def get(t:JType):Option[T] =
    Lens.getValue(t, path.segments).flatMap(pattern.unapply)

  val ? = Maybe(unapply)

  def validate(value: Option[JType], currentState: Option[JType], path:Path): Seq[ValidationFailure] = {
    val typeMatch =
      value.collect {
        case j if j != JNull && pattern.unapply(j).isEmpty =>
          UnexpectedTypeFailure(path, pattern.toString)
      }

    val valid =  validator.validate(value, currentState, path)
    val properties = contractProperties.flatMap(p => p.validate(value \ p.key, currentState \ p.key, path \ p.key))
    valid ++ typeMatch  ++ properties
  }

  def set =
    (value:T) => (j:JType) => Lens.setValue(Some(j), path.segments, pattern(value))
  def modify =
    (func:T => T) => (j:JType) =>
      Lens.getValue(j, path.segments)
      .flatMap(pattern.unapply)
      .fold[JType](j)(v => Lens.setValue(Some(j), path.segments, pattern(func(v))))
  def maybeModify =
    (func:Option[T] => T) => (j:JType) => {
    val current = Lens.getValue(j, path.segments).flatMap(pattern.unapply)
    Lens.setValue(Some(j), path.segments, pattern(func(current)))
  }
  def clear =
    (j:JType) => Lens.removeValue(j, path.segments)
  def move =
    (p:Property[T]) => (j:JType) => {
      Lens.getValue(j, path.segments) match {
        case None =>
          Lens.removeValue(j, p.path.segments)
        case Some(value) =>
          val j2 = Lens.removeValue(j, path.segments)
          Lens.setValue(Some(j2), p.path.segments, value)
      }
    }
}

case class Maybe[T](f:JType => Option[T]) {
  def unapply(t:JType) =
    Some(f(t))
}

trait Pattern[T] {
  def unapply(json:JType):Option[T]
  def apply(t:T):JType
}

trait JsonPatterns {

  implicit val string = new Pattern[String] {
    def unapply(json: JType): Option[String] =
      json match {
        case JText(j) => Some(j)
        case _ => None
      }

    def apply(t: String): JType =
      JText(t)
  }

  implicit val long = new Pattern[Long] {
    def unapply(json: JType): Option[Long] =
      json match {
        case JLong(j) => Some(j)
        case _ => None
      }

    def apply(t: Long): JType =
      JLong(t)
  }

  implicit val map = new Pattern[JMap] {
    def unapply(json: JType): Option[JMap] =
      json match {
        case JObject(j) => Some(j)
        case _ => None
      }

    def apply(t:JMap): JType =
      JObject(t)
  }

  implicit def seq[T](implicit pattern:Pattern[T]) = new Pattern[Seq[T]] {
    def unapply(json: JType): Option[Seq[T]] =
      json match {
        case JArray(j) => Some(j.collect{ case pattern(e) => e})
        case _ => None
      }

    def apply(t: Seq[T]): JType =
      JArray(t.map(pattern.apply))
  }

  implicit def tuple[T1,T2](implicit pattern1:Pattern[T1], pattern2:Pattern[T2]) = new Pattern[(T1, T2)] {
    def unapply(json: JType): Option[(T1, T2)] =
      json match {
        case JArray(pattern1(j1) +: pattern2(j2) +: _) => Some(j1 -> j2)
        case _ => None
      }

    def apply(t: (T1, T2)): JType =
      JArray(Seq(pattern1(t._1), pattern2(t._2)))
  }
}

object DefaultPatterns extends JsonPatterns
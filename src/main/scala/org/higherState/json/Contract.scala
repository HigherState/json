package org.higherState.json

import reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror=>cm,universe=>ru}

trait SelfApply {
  def apply[R](f:this.type => R):R = f(this)
}

trait BaseContract extends SelfApply {
  implicit protected def path:Path
  def pattern:Pattern[JMap]

  lazy val contractProperties:Seq[Property[_]] = {
    val r = cm.reflect(this)
    val t = ru.appliedType(r.symbol.asType.toType)
    val propertyErasure = typeOf[Property[_]].erasure
    t.members.collect{ case m:MethodSymbol if m.isPublic && m.returnType <:< propertyErasure  && !m.isConstructor =>
      r.reflectMethod(m)().asInstanceOf[Property[_]]// reflectField doesnt work oddly
    }.toSeq
  }
}

abstract class Contract(implicit p:Pattern[JMap]) extends BaseContract {
  implicit protected def path:Path = Path.empty
  def pattern = p

  def unapply(j:Json):Option[JMap] =
    pattern.unapply(j)
}

abstract class ContractType(key:String, value:Option[Json] = None)(implicit p:Pattern[JMap]) extends BaseContract {
  implicit protected def path: Path = Path.empty
  def pattern = p

  def unapply(j:Json):Option[JMap] =
    pattern.unapply(j).filter(_.get(key).exists(j => value.fold(true)(_ == j)))
}


trait SubContract {
  implicit protected def path:Path
}

trait Property[T <: Any] extends SelfApply {
  def pattern:Pattern[T]
  def path:Path
  def key:String
  def validator:Validator[T]
}

abstract class Expected[T](key:String)(implicit parentPath:Path, val pattern:Pattern[T]) extends Property[T] {
  import JsonPath._
  val path:Path = parentPath \ key

  def unapply(j:Json):Option[T] =
    getValue(j, path.segments).flatMap(pattern.unapply)
}

abstract class Maybe[T](key:String)(implicit parentPath:Path, val pattern:Pattern[T]) extends Property[T] {
  import JsonPath._
  val path:Path = parentPath \ key

  def unapply(j:Json):Option[Option[T]] =
    Some(getValue(j, path.segments).flatMap(pattern.unapply))
}

abstract class Default[T](key:String, default:T)(implicit parentPath:Path, val pattern:Pattern[T]) extends Property[T] {
  import JsonPath._
  val path:Path = parentPath \ key

  def unapply(j:Json):Option[T] =
    Some(getValue(j, path.segments).flatMap(pattern.unapply).getOrElse(default))
}

case class \[T](key:String, validator:Validator[T] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[T]) extends Expected[T](key)(parentPath, pattern)

case class \?[T](key:String, validator:Validator[T] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[T]) extends Maybe[T](key)(parentPath, pattern)

case class \![T](key:String, default:T, validator:Validator[T] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[T]) extends Default[T](key, default)(parentPath, pattern)

abstract class \\(val key:String, val validator:Validator[JMap] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[JMap]) extends Expected[JMap](key)(parentPath, pattern) with BaseContract

abstract class \\?(val key:String, val validator:Validator[JMap] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[JMap]) extends Maybe[JMap](key)(parentPath, pattern) with BaseContract

case class \:[T](key:String, validator:Validator[Seq[T]] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[Seq[T]], val seqPattern:Pattern[Seq[Json]], val elementPattern:Pattern[T]) extends Expected[Seq[T]](key)(parentPath, pattern)

case class \:?[T](key:String, validator:Validator[Seq[T]] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[Seq[T]], val seqPattern:Pattern[Seq[Json]], val elementPattern:Pattern[T]) extends Maybe[Seq[T]](key)(parentPath, pattern)
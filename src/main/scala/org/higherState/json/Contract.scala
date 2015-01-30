package org.higherState.json

import reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror=>cm,universe=>ru}

trait SelfApply {
  def apply[R](f:this.type => R):R = f(this)
}

trait BaseContract extends SelfApply {
  implicit protected def path:Path
 // def pattern:Pattern[T]

  lazy val contractProperties:Seq[Property[_]] = {
    val r = cm.reflect(this)
    val t = ru.appliedType(r.symbol.asType.toType)
    val propertyErasure = typeOf[Property[_]].erasure
    t.members.collect{ case m:MethodSymbol if m.isPublic && m.returnType <:< propertyErasure  && !m.isConstructor =>
      r.reflectMethod(m)().asInstanceOf[Property[_]]// reflectField doesnt work oddly
    }.toSeq
  }
}

abstract class Contract(implicit pattern:Pattern[JObject]) extends BaseContract {
  implicit protected def path:Path = Path.empty

  def unapply(j:Json):Option[JObject] =
    pattern.unapply(j)

  def create(f:this.type => Json => Json):Json = f(this)(JObject.empty)
}

abstract class ContractType(key:String, matcher:Matcher = DefaultMatcher)(implicit pattern:Pattern[JObject]) extends BaseContract {
  implicit protected def path: Path = Path.empty
  def unapply(j:Json):Option[JObject] =
    pattern.unapply(j).filter(_.value.get(key).exists(matcher.isMatch))

  def create(f:this.type => Json => Json):Json = f(this)(JObject(Map(key -> matcher.default)))
  def create() = JObject(Map(key -> matcher.default))
}

trait Matcher  {
  def isMatch(j:Json):Boolean
  def default:Json
}

object DefaultMatcher extends Matcher {
  def isMatch(j:Json):Boolean = true
  def default:Json = JNull
}

object JsonMatchers {
  implicit def valueMatcher[T](value:T)(implicit pattern:Pattern[T]) = new Matcher {
    val default: Json = pattern.apply(value)
    def isMatch(j: Json): Boolean = j == default
  }
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
  implicit val path:Path = parentPath \ key

  def unapply(j:Json):Option[T] =
    getValue(j, path.segments).flatMap(pattern.unapply)
}

abstract class Maybe[T](key:String)(implicit parentPath:Path, val pattern:Pattern[Option[T]]) extends Property[Option[T]] {
  import JsonPath._
  implicit val path:Path = parentPath \ key

  def unapply(j:Json):Option[Option[T]] =
    getValue(j, path.segments).fold[Option[Option[T]]](Some(None)) { v =>
      pattern.unapply(v) //always returns Some()
    }

}

abstract class Default[T](key:String, default:T)(implicit parentPath:Path, val pattern:Pattern[Option[T]]) extends Property[Option[T]] {
  import JsonPath._

  implicit val path:Path = parentPath \ key

  def unapply(j:Json):Option[T] =
    getValue(j, path.segments).fold[Option[T]](Some(default)) { v =>
      pattern.unapply(v).map(_.getOrElse(default)) //always returns Some()
    }
}

case class \[T](key:String, validator:Validator[T] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[T]) extends Expected[T](key)(parentPath, pattern)

case class \?[T](key:String, validator:Validator[Option[T]] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[Option[T]]) extends Maybe[T](key)(parentPath, pattern)

case class \![T](key:String, default:T, validator:Validator[Option[T]] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[Option[T]]) extends Default[T](key, default)(parentPath, pattern)

abstract class \\(val key:String, val validator:Validator[JMap] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[JMap]) extends Expected[JMap](key)(parentPath, pattern) with BaseContract

abstract class \\?(val key:String, val validator:Validator[Option[JMap]] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[Option[JMap]]) extends Maybe[JMap](key)(parentPath, pattern) with BaseContract

case class \:[T](key:String, validator:Validator[Seq[T]] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[Seq[T]], val seqPattern:Pattern[Seq[Json]], val elementPattern:Pattern[T]) extends Expected[Seq[T]](key)(parentPath, pattern)

case class \:?[T](key:String, validator:Validator[Option[Seq[T]]] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[Option[Seq[T]]], val seqPattern:Pattern[Seq[Json]], val elementPattern:Pattern[T]) extends Maybe[Seq[T]](key)(parentPath, pattern)
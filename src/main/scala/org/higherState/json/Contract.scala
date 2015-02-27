package org.higherState.json

import reflect.runtime.universe._
import scala.reflect.runtime.{universe=>ru}

trait SelfApply {
  def apply[R](f:this.type => R):R = f(this)
}

trait BaseContract extends SelfApply {
  implicit protected def absolutePath:Path

  lazy val contractProperties: Seq[Property[_]] = {
    val mirror = ru.runtimeMirror(this.getClass.getClassLoader)
    val r = mirror.reflect(this)
    val t = ru.appliedType(r.symbol.asType.toType, Nil)
    val propertyErasure = typeOf[Property[_]].erasure
    t.members.collect { case m: MethodSymbol if m.returnType.erasure <:< propertyErasure && !m.isConstructor =>
      r.reflectMethod(m)().asInstanceOf[Property[_]] // reflectField doesnt work oddly
    }.toSeq
  }
}

abstract class Contract(implicit pattern:Pattern[JObject]) extends BaseContract {
  implicit protected def absolutePath:Path = Path.empty

  def unapply(j:Json):Option[JObject] =
    pattern.unapply(j)

  def create(f:this.type => Json => Json):Json = f(this)(JObject.empty)
}

abstract class ContractType(key:String, matcher:Matcher = DefaultMatcher)(implicit pattern:Pattern[JObject]) extends BaseContract {
  implicit protected def absolutePath: Path = Path.empty
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
  implicit protected def absolutePath:Path
}

trait Property[T <: Any] extends SelfApply {
  def pattern:Pattern[T]
  def absolutePath:Path
  def relativePath:Path
  def validator:Validator[T]
}

class Expected[T](val relativePath:Path, implicit val absolutePath:Path, val validator:Validator[T])(implicit val pattern:Pattern[T]) extends Property[T] {
  import JsonPath._

  def unapply(j:Json):Option[T] =
    getValue(j, absolutePath.segments).flatMap(pattern.unapply)
}

class Maybe[T](val relativePath:Path, implicit val absolutePath:Path, val validator:Validator[Option[T]])(implicit val pattern:Pattern[Option[T]]) extends Property[Option[T]] {
  import JsonPath._

  def unapply(j:Json):Option[Option[T]] =
    getValue(j, absolutePath.segments).fold[Option[Option[T]]](Some(None)) { v =>
      pattern.unapply(v) //always returns Some()
    }

}

class Default[T](val relativePath:Path, implicit val absolutePath:Path, default:T, val validator:Validator[Option[T]])(implicit val pattern:Pattern[Option[T]]) extends Property[Option[T]] {
  import JsonPath._

  def unapply(j:Json):Option[T] =
    getValue(j, absolutePath.segments).fold[Option[T]](Some(default)) { v =>
      pattern.unapply(v).map(_.getOrElse(default)) //always returns Some()
    }
}

object \ {
  def apply[T](path:Path, validator:Validator[T] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[T]) =
    new Expected[T](path, parentPath ++ path, validator)(pattern)
}

object \? {

  def apply[T](path:Path, validator:Validator[Option[T]] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[Option[T]]) =
    new Maybe[T](path, parentPath ++ path, validator)(pattern)
}

object \! {
  def apply[T](path:Path, default:T, validator:Validator[Option[T]] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[Option[T]]) =
    new Default[T](path, parentPath ++ path, default, validator)(pattern)
}

abstract class \\(path:Path, validator:Validator[JObject] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[JObject])
  extends Expected[JObject](path, parentPath ++ path, validator)(pattern) with BaseContract

abstract class \\?(path:Path, validator:Validator[Option[JObject]] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[Option[JObject]])
  extends Maybe[JObject](path, parentPath ++ path, validator)(pattern) with BaseContract

case class \:[T](path:Path, override val validator:Validator[Seq[T]] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[Seq[T]], val seqPattern:Pattern[Seq[Json]], val elementPattern:Pattern[T])
  extends Expected[Seq[T]](path, parentPath ++ path, validator)(pattern)

case class \:?[T](path:Path, override val validator:Validator[Option[Seq[T]]] = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[Option[Seq[T]]], val seqPattern:Pattern[Seq[Json]], val elementPattern:Pattern[T])
  extends Maybe[Seq[T]](path, parentPath ++ path, validator)(pattern)
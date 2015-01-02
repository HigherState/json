package org.higherState.json

import reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror=>cm,universe=>ru}
import org.higherState.validation.ValidationFailure
import JsonConstructor._
import JsonPath._

abstract class Contract extends PropertyMapper with Validator {
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

  def getSchema:JObject
}

class Property[T <: Any](val key:String, validator:Validator = EmptyValidator)(implicit parentPath:Path, pattern:Pattern[T]) extends PropertyMapper {
  implicit protected val path:Path = parentPath \ key

  def unapply(t:Json):Option[T] =
    get(t)

  def apply[R](f:this.type => R):R = f(this)

  def get(t:Json):Option[T] =
    Lens.getValue(t, path.segments).flatMap(pattern.unapply)

  val ? = Maybe(unapply)

  def validate(value: Option[Json], currentState: Option[Json], path:Path): Seq[ValidationFailure] = {
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
    (p:Property[T]) => (j:Json) => {
      Lens.getValue(j, path.segments) match {
        case None =>
          Lens.removeValue(j, p.path.segments)
        case Some(value) =>
          val j2 = Lens.removeValue(j, path.segments)
          Lens.setValue(Some(j2), p.path.segments, value)
      }
    }


  def getSchema:JObject =
    if (contractProperties.isEmpty)
      pattern.getSchema ++ validator.getSchema
    else
      pattern.getSchema ++ validator.getSchema + ("properties" -> JObject(contractProperties.map(p => p.key -> p.getSchema):_*))
}

case class Maybe[T](f:Json => Option[T]) {
  def unapply(t:Json) =
    Some(f(t))
}

trait Pattern[T] {
  def unapply(json:Json):Option[T]
  def apply(t:T):Json
  def getSchema:JObject
}

trait JsonPatterns {

  val TYPE = "type"
  val ITEMS = "items"
  val PROPERTIES = "properties"

  implicit val string = new Pattern[String] {
    def unapply(json: Json): Option[String] =
      json match {
        case JString(j) => Some(j)
        case _ => None
      }

    def apply(t: String): Json =
      JString(t)

    def getSchema: JObject = JObject(TYPE -> "string".j)
  }

  implicit val long = new Pattern[Long] {
    def unapply(json: Json): Option[Long] =
      json match {
        case JLong(j) => Some(j)
        case JDouble(j) if j % 1 == 0 => Some(j.toLong)
        case _ => None
      }

    def apply(t: Long): Json =
      JLong(t)

    def getSchema: JObject = JObject(TYPE -> "long".j)
  }
  implicit val int = new Pattern[Int] {
    def unapply(json: Json): Option[Int] =
      json match {
        case JLong(j) if j >= Int.MinValue && j <= Int.MaxValue =>
          Some(j.toInt)
        case JDouble(j) if j >= Int.MinValue && j <= Int.MaxValue  && j % 1 == 0 =>
          Some(j.toInt)
        case _ => None
      }

    def apply(t: Int): Json =
      JLong(t)

    def getSchema: JObject = JObject(TYPE -> "int".j)
  }
  implicit val double = new Pattern[Double] {
    def unapply(json: Json): Option[Double] =
      json match {
        case JDouble(j) => Some(j)
        case JLong(j) => Some(j)
        case _ => None
      }

    def apply(t: Double): Json =
      JDouble(t)

    def getSchema: JObject = JObject(TYPE -> "double".j)
  }
  implicit val float = new Pattern[Float] {
    def unapply(json: Json): Option[Float] =
      json match {
        case JDouble(j) if j >= Float.MinValue && j <= Float.MaxValue => Some(j.toFloat)
        case JLong(j) => Some(j)
        case _ => None
      }

    def apply(t: Float): Json =
      JDouble(t)

    def getSchema: JObject = JObject(TYPE -> "float".j)
  }

  implicit val map = new Pattern[JMap] {
    def unapply(json: Json): Option[JMap] =
      json match {
        case JObject(j) => Some(j)
        case _ => None
      }

    def apply(t:JMap): Json =
      JObject(t)

    def getSchema: JObject = JObject(TYPE -> "object".j)
  }

  implicit val obj = new Pattern[JObject] {
    def unapply(json: Json): Option[JObject] =
      json match {
        case j:JObject => Some(j)
        case _ => None
      }

    def apply(t:JObject): Json = t
    def getSchema: JObject = JObject(TYPE -> "object".j)
  }

  implicit def seq[T](implicit pattern:Pattern[T]) = new Pattern[Seq[T]] {
    def unapply(json: Json): Option[Seq[T]] =
      json match {
        case JArray(j) => Some(j.collect{ case pattern(e) => e})
        case _ => None
      }

    def apply(t: Seq[T]): Json =
      JArray(t.map(pattern.apply))

    def getSchema: JObject = JObject(TYPE -> "array".j, ITEMS -> pattern.getSchema)
  }

  implicit def tuple[T1,T2](implicit pattern1:Pattern[T1], pattern2:Pattern[T2]) = new Pattern[(T1, T2)] {
    def unapply(json: Json): Option[(T1, T2)] =
      json match {
        case JArray(pattern1(j1) +: pattern2(j2) +: _) => Some(j1 -> j2)
        case _ => None
      }

    def apply(t: (T1, T2)): Json =
      JArray(Seq(pattern1(t._1), pattern2(t._2)))

    def getSchema: JObject =
      JObject(TYPE -> "array".j, ITEMS -> JArray(pattern1.getSchema, pattern2.getSchema))
  }
}

object DefaultPatterns extends JsonPatterns
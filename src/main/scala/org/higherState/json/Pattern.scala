package org.higherState.json

import java.util.UUID

import org.higherState.validation.ValidationFailure

trait Pattern[T] extends Validator {
  protected def extractor:PartialFunction[Json, T]
  def apply(t:T):Json
  def schema:JObject

  def unapply(json:Json):Option[T] =
    extractor.lift(json)
}


trait JsonPatterns {
  import JsonConstructor._

  val TYPE = "type"
  val ITEMS = "items"
  val PROPERTIES = "properties"

  sealed abstract class ValuePattern[T](typeName:String) extends Pattern[T] {
    def validate(value: Option[Json], currentState: Option[Json], path: Path): Seq[ValidationFailure] =
      value.collect {
        case v if !extractor.isDefinedAt(v) =>
          UnexpectedTypeFailure(path, this, v.getClass.getSimpleName)
      }.toSeq

    def schema: JObject = JObject(TYPE -> "string".j)
  }

  implicit val string = new ValuePattern[String]("string"){

    protected def extractor = {
      case JString(j) => j
    }
    def apply(t: String): Json =
      JString(t)
  }

  implicit val long = new ValuePattern[Long]("long") {
    protected def extractor = {
      case JLong(j) => j
      case JDouble(j) if j % 1 == 0 && j <= Long.MaxValue && j >= Long.MinValue => j.toLong
    }

    def apply(t: Long): Json =
      JLong(t)
  }
  implicit val int = new ValuePattern[Int]("int") {
    protected def extractor = {
      case JLong(j) if j >= Int.MinValue && j <= Int.MaxValue =>
        j.toInt
      case JDouble(j) if j >= Int.MinValue && j <= Int.MaxValue  && j % 1 == 0 =>
        j.toInt
    }

    def apply(t: Int): Json =
      JLong(t)
  }
  implicit val double = new ValuePattern[Double]("double") {
    protected def extractor = {
      case JDouble(j) => j
      case JLong(j) => j
    }

    def apply(t: Double): Json =
      JDouble(t)
  }
  implicit val float = new ValuePattern[Float]("float") {
    protected def extractor = {
      case JDouble(j) if j >= Float.MinValue && j <= Float.MaxValue => j.toFloat
      case JLong(j) => j
    }

    def apply(t: Float): Json =
      JDouble(t)
  }

  protected val uuidRegex = "([a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12})".r

  implicit val uuid = new ValuePattern[UUID]("guid") {
    override protected def extractor: PartialFunction[Json, UUID] = {
      case JString(uuidRegex(v)) => UUID.fromString(v)
    }

    override def apply(t: UUID): Json =
      JString(t.toString)
  }

  implicit val json = new ValuePattern[Json]("json"){
    protected def extractor = {
      case j => j
    }
    def apply(t:Json): Json = t
  }

  implicit val jobj = new ValuePattern[JObject]("object"){
    protected def extractor = {
      case j:JObject => j
    }
    def apply(t:JObject): Json = t
  }

  implicit def seq[T](implicit pattern:Pattern[T]) = new Pattern[Seq[T]] {
    protected def extractor = {
      case JArray(j) => j.collect{ case pattern(e) => e}
    }

    def validate(value: Option[Json], currentState: Option[Json], path: Path): Seq[ValidationFailure] =
      value.toSeq.flatMap {
        case JArray(v) =>
          v.zipWithIndex.flatMap(e => pattern.validate(Some(e._1), None, path \ e._2))
        case v =>
          Some(UnexpectedTypeFailure(path, this, v.getClass.getSimpleName))
      }

    def apply(t: Seq[T]): Json =
      JArray(t.map(pattern.apply))

    def schema: JObject = JObject(TYPE -> "array".j, ITEMS -> pattern.schema)
  }

  implicit def tuple[T1,T2](implicit pattern1:Pattern[T1], pattern2:Pattern[T2]) = new Pattern[(T1, T2)] {
    protected def extractor = {
      case JArray(pattern1(j1) +: pattern2(j2) +: _) => j1 -> j2
    }

    def apply(t: (T1, T2)): Json =
      JArray(Seq(pattern1(t._1), pattern2(t._2)))

    def validate(value: Option[Json], currentState: Option[Json], path: Path): Seq[ValidationFailure] =
      value.collect {
        case v if !extractor.isDefinedAt(v) =>
          UnexpectedTypeFailure(path, this, v.getClass.getSimpleName)
      }.toSeq

    def schema: JObject =
      JObject(TYPE -> "array".j, ITEMS -> JArray(pattern1.schema, pattern2.schema))
  }

  implicit def either[T1,T2] (implicit pattern1:Pattern[T1], pattern2:Pattern[T2]) = new Pattern[Either[T1, T2]] {
    protected def extractor = {
      case pattern1(j) => Left(j)
      case pattern2(j) => Right(j)
    }

    def apply(t: Either[T1, T2]): Json =
      t match {
        case Left(t1) => pattern1.apply(t1)
        case Right(t2) => pattern2.apply(t2)
      }

    def validate(value: Option[Json], currentState: Option[Json], path: Path): Seq[ValidationFailure] =
      value.collect {
        case v if !extractor.isDefinedAt(v) =>
          UnexpectedTypeFailure(path, this, v.getClass.getSimpleName)
      }.toSeq

    def schema: JObject = JObject(TYPE -> JArray(pattern1.schema, pattern2.schema))
  }
}

object DefaultPatterns extends JsonPatterns

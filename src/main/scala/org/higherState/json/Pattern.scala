package org.higherState.json

trait Pattern[T] {
  protected def extractor:PartialFunction[Json, T]
  def apply(t:T):Json
  def schema:JObject

  def unapply(json:Json):Option[T] =
    extractor.lift(json)
}
object JsonSchema {
  val TYPE = "type"
  val ITEMS = "items"
  val PROPERTIES = "properties"
  val REQUIRED = "required"
  val DEFAULT = "default"
}
abstract class RequiredValuePattern[T](typeName: String) extends Pattern[T] {
  import JsonSchema._

  def validate(value: Option[Json], currentState: Option[Json], path: Path): Seq[(String, Path)] =
    (value, currentState) match {
      case (None, None) =>
        Seq("Value expected." -> path)
      case (Some(v), _) if !extractor.isDefinedAt(v) =>
        Seq(s"Unexpected type '${v.getClass.getSimpleName}'." -> path)
      case _ =>
        Nil
    }

  def schema: JObject = JObject(TYPE -> JString(typeName), REQUIRED -> JTrue)
}

trait SeqExtractor[T] {
  def unapply(s:Seq[Json]):Option[Seq[T]]
}

trait JsonConstructor {
  implicit class ToJ[T](val value:T)(implicit pattern:Pattern[T]) {
    def j = pattern(value)
  }
}

object JsonConstructor extends JsonConstructor

trait JsonPatterns {

  import JsonSchema._

  implicit val booleanPattern:Pattern[Boolean] = new RequiredValuePattern[Boolean]("boolean"){

    protected def extractor = {
      case JBool(j) => j
    }
    def apply(t: Boolean): Json =
      JBool(t)
  }

  implicit val stringPattern:Pattern[String] = new RequiredValuePattern[String]("string") {

    protected def extractor = {
      case JString(j) => j
    }

    def apply(t: String): Json =
      JString(t)
  }

  implicit val longPattern:Pattern[Long] = new RequiredValuePattern[Long]("long") {
    protected def extractor = {
      case JLong(j) => j
      case JDouble(j) if j % 1 == 0 && j <= Long.MaxValue && j >= Long.MinValue => j.toLong
    }

    def apply(t: Long): Json =
      JLong(t)
  }
  implicit val intPattern:Pattern[Int] = new RequiredValuePattern[Int]("int") {
    protected def extractor = {
      case JLong(j) if j >= Int.MinValue && j <= Int.MaxValue =>
        j.toInt
      case JDouble(j) if j >= Int.MinValue && j <= Int.MaxValue && j % 1 == 0 =>
        j.toInt
    }

    def apply(t: Int): Json =
      JLong(t)
  }
  implicit val doublePattern:Pattern[Double] = new RequiredValuePattern[Double]("double") {
    protected def extractor = {
      case JDouble(j) => j
      case JLong(j) => j
    }

    def apply(t: Double): Json =
      JDouble(t)
  }
  implicit val float:Pattern[Float] = new RequiredValuePattern[Float]("float") {
    protected def extractor = {
      case JDouble(j) if j >= Float.MinValue && j <= Float.MaxValue => j.toFloat
      case JLong(j) => j
    }

    def apply(t: Float): Json =
      JDouble(t)
  }

  implicit val jsonPattern:Pattern[Json] = new RequiredValuePattern[Json]("json"){
    protected def extractor = {
      case j => j
    }
    def apply(t:Json): Json = t
  }

  implicit val jObjPattern:Pattern[JObject] = new RequiredValuePattern[JObject]("object"){
    protected def extractor = {
      case j:JObject => j
    }
    def apply(t:JObject): Json = t
  }

  implicit val jMapPattern:Pattern[JMap] = new RequiredValuePattern[JMap]("object"){
    protected def extractor = {
      case JObject(j) => j
    }
    def apply(t:JMap): Json = JObject(t)
  }
  implicit val jArrayPattern:Pattern[Array[Json]] = new RequiredValuePattern[Array[Json]]("array"){
    protected def extractor = {
      case JArray(j) => j.toArray
    }
    def apply(t:Array[Json]): Json = JArray(t)
  }
  implicit val jSeqPattern:Pattern[Seq[Json]] = new RequiredValuePattern[Seq[Json]]("array"){
    protected def extractor = {
      case JArray(j) => j
    }
    def apply(t:Seq[Json]): Json = JArray(t)
  }
  implicit val jListPattern:Pattern[List[Json]] = new RequiredValuePattern[List[Json]]("array"){
    protected def extractor = {
      case JArray(j) => j.toList
    }
    def apply(t:List[Json]): Json = JArray(t)
  }
  implicit val jVectorPattern:Pattern[Vector[Json]] = new RequiredValuePattern[Vector[Json]]("array"){
    protected def extractor = {
      case JArray(j) => j.toVector
    }
    def apply(t:Vector[Json]): Json = JArray(t)
  }
  implicit val jSetPattern:Pattern[Set[Json]] = new RequiredValuePattern[Set[Json]]("array"){
    protected def extractor = {
      case JArray(j) => j.toSet
    }
    def apply(t:Set[Json]): Json = JArray(t.toSeq)
  }

  implicit def allExtractedPattern[T](implicit pattern:Pattern[T]) = new SeqExtractor[T] {
    def unapply(j:Seq[Json]):Option[Seq[T]] = {
      val t = j.toIterator.map(pattern.unapply).takeWhile(_.isDefined).flatten.toSeq
      if (t.size == j.size) Some(t)
      else None
    }
  }

  //all or nothing extraction
  implicit def seqPattern[T](implicit sqlExtractor:SeqExtractor[T], pattern:Pattern[T]) = new Pattern[Seq[T]] {
    protected def extractor = {
      case JArray(sqlExtractor(j)) => j
    }

    def apply(t: Seq[T]): Json =
      JArray(t.map(pattern.apply))

    def schema: JObject = JObject(TYPE -> JString("array"), ITEMS -> pattern.schema)
  }

  implicit def setPattern[T](implicit sqlExtractor:SeqExtractor[T], pattern:Pattern[T]) = new Pattern[Set[T]] {
    protected def extractor = {
      case JArray(sqlExtractor(j)) => j.toSet
    }

    def apply(t: Set[T]): Json =
      JArray(t.map(pattern.apply).toSeq)

    def schema: JObject = JObject(TYPE -> JString("array"), ITEMS -> pattern.schema)
  }

  implicit def mapPattern[T](implicit pattern:Pattern[T]) = new Pattern[Map[String, T]] {
    protected def extractor: PartialFunction[Json, Map[String, T]] = {
      case JObject(m) => m.collect{ case (k, pattern(v)) => k -> v}
    }

    def apply(t: Map[String, T]): Json =
      JObject(t.mapValues(pattern.apply))

    def schema: JObject = ???
  }

  implicit def optionPattern[T](implicit pattern:Pattern[T]) = new Pattern[Option[T]] {
    def schema: JObject = JObject(pattern.schema.value + (REQUIRED -> JFalse))

    def apply(t: Option[T]): Json = t.fold[Json](JNull)(pattern.apply)

    protected def extractor: PartialFunction[Json, Option[T]] = {
      case JNull => None
      case j => pattern.unapply(j)
    }
  }

  implicit def tuplePattern[T1,T2](implicit pattern1:Pattern[T1], pattern2:Pattern[T2]) = new Pattern[(T1, T2)] {
    protected def extractor = {
      case JArray(pattern1(j1) +: pattern2(j2) +: _) => j1 -> j2
    }

    def apply(t: (T1, T2)): Json =
      JArray(Seq(pattern1(t._1), pattern2(t._2)))

    def schema: JObject =
      JObject(TYPE -> JString("array"), ITEMS -> JArray(pattern1.schema, pattern2.schema))
  }

  implicit def eitherPattern[T1,T2] (implicit pattern1:Pattern[T1], pattern2:Pattern[T2]) = new Pattern[Either[T1, T2]] {
    protected def extractor = {
      case pattern1(j) => Left(j)
      case pattern2(j) => Right(j)
    }

    def apply(t: Either[T1, T2]): Json =
      t match {
        case Left(t1) => pattern1.apply(t1)
        case Right(t2) => pattern2.apply(t2)
      }

    def schema: JObject = JObject(TYPE -> JArray(pattern1.schema, pattern2.schema))
  }
}


object DefaultPatterns extends JsonPatterns

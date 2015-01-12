package org.higherState.json

trait Pattern[T] {
  protected def extractor:PartialFunction[Json, T]
  def unapply(json:Json):Option[T] =
    extractor.lift(json)
  def apply(t:T):Json
  def getSchema:JObject
}

trait JsonPatterns {
  import JsonConstructor._

  val TYPE = "type"
  val ITEMS = "items"
  val PROPERTIES = "properties"

  implicit val string = new Pattern[String] {
    protected def extractor = {
      case JString(j) => j
    }
    def apply(t: String): Json =
      JString(t)

    def getSchema: JObject = JObject(TYPE -> "string".j)
  }

  implicit val long = new Pattern[Long] {
    protected def extractor = {
      case JLong(j) => j
      case JDouble(j) if j % 1 == 0 && j <= Long.MaxValue && j >= Long.MinValue => j.toLong
    }

    def apply(t: Long): Json =
      JLong(t)

    def getSchema: JObject = JObject(TYPE -> "long".j)
  }
  implicit val int = new Pattern[Int] {
    protected def extractor = {
      case JLong(j) if j >= Int.MinValue && j <= Int.MaxValue =>
        j.toInt
      case JDouble(j) if j >= Int.MinValue && j <= Int.MaxValue  && j % 1 == 0 =>
        j.toInt
    }

    def apply(t: Int): Json =
      JLong(t)

    def getSchema: JObject = JObject(TYPE -> "int".j)
  }
  implicit val double = new Pattern[Double] {
    protected def extractor = {
      case JDouble(j) => j
      case JLong(j) => j
    }

    def apply(t: Double): Json =
      JDouble(t)

    def getSchema: JObject = JObject(TYPE -> "double".j)
  }
  implicit val float = new Pattern[Float] {
    protected def extractor = {
      case JDouble(j) if j >= Float.MinValue && j <= Float.MaxValue => j.toFloat
      case JLong(j) => j
    }

    def apply(t: Float): Json =
      JDouble(t)

    def getSchema: JObject = JObject(TYPE -> "float".j)
  }

  implicit val map = new Pattern[JMap] {
    protected def extractor = {
      case JObject(j) => j
    }

    def apply(t:JMap): Json =
      JObject(t)

    def getSchema: JObject = JObject(TYPE -> "object".j)
  }

  implicit val obj = new Pattern[JObject] {
    protected def extractor = {
      case j:JObject => j
    }

    def apply(t:JObject): Json = t
    def getSchema: JObject = JObject(TYPE -> "object".j)
  }

  implicit def seq[T](implicit pattern:Pattern[T]) = new Pattern[Seq[T]] {
    protected def extractor = {
      case JArray(j) => j.collect{ case pattern(e) => e}
    }

    def apply(t: Seq[T]): Json =
      JArray(t.map(pattern.apply))

    def getSchema: JObject = JObject(TYPE -> "array".j, ITEMS -> pattern.getSchema)
  }

  implicit def tuple[T1,T2](implicit pattern1:Pattern[T1], pattern2:Pattern[T2]) = new Pattern[(T1, T2)] {
    protected def extractor = {
      case JArray(pattern1(j1) +: pattern2(j2) +: _) => j1 -> j2
    }

    def apply(t: (T1, T2)): Json =
      JArray(Seq(pattern1(t._1), pattern2(t._2)))

    def getSchema: JObject =
      JObject(TYPE -> "array".j, ITEMS -> JArray(pattern1.getSchema, pattern2.getSchema))
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

    def getSchema: JObject = JObject(TYPE -> JArray(pattern1.getSchema, pattern2.getSchema))
  }
}

object DefaultPatterns extends JsonPatterns

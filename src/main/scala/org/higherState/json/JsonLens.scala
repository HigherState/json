package org.higherState.json


object JsonLens {
  import JsonPath._

  implicit class Set(val f: Json => Json) extends AnyVal {
    def ~(f2: Json => Json): Json => Json =
      (j: Json) => f2(f(j))
  }

//  implicit class Unset[T](val f: T => Json => Json) extends AnyVal {
//    def ~(f2: Json => Json): Json => Json =
//      (j: Json) => f2(f(j))
//  }



  implicit class ValueLens[T](val prop: Property[T]) extends AnyVal {

    def get(j:Json):Option[T] =
      getValue(j, prop.path.segments).flatMap(prop.pattern.unapply)
    def set =
      (value:T) => (j:Json) => setValue(Some(j), prop.path.segments, prop.pattern(value))
    def modify =
      (func:T => T) => (j:Json) =>
       get(j).fold[Json](j)(v => setValue(Some(j), prop.path.segments, prop.pattern(func(v))))
    def maybeModify =
      (func:Option[T] => T) => (j:Json) =>
        setValue(Some(j), prop.path.segments, prop.pattern(func(get(j))))
    def copy =
      (p:Property[T]) => (j:Json) => {
        getValue(j, prop.path.segments) match {
          case None =>
          case Some(value) =>
            insertValue(Some(j), p.path.segments, value)
        }
      }
  }

  implicit class MaybeLens[T](val prop: Property[Option[T]]) extends AnyVal {
    def drop =
      (j:Json) => dropValue(j, prop.path.segments)
//    def move =
//      (p:Property[T]) => (j:Json) => {
//        getValue(j, prop.path.segments) match {
//          case None =>
//            dropValue(j, prop.path.segments)
//          case Some(value) =>
//            val j2 = dropValue(j, prop.path.segments)
//            insertValue(Some(j2), p.path.segments, value)
//        }
//      }
  }

  implicit class ArrayLens[T](val prop: \:[T]) extends AnyVal {
    def at(index:Int) = ArrayElement[T](index, prop.path)(prop.elementPattern)

    def head = ArrayElement[T](0, prop.path)(prop.elementPattern)

    def append =
      (value:T) => (j:Json) =>
        setValue(Some(j), prop.path.segments, prop.seqPattern.apply(current(j) :+ prop.elementPattern(value)))

    def prepend =
      (value:T) => (j:Json) =>
        prop.seqPattern.apply(prop.elementPattern(value) +: current(j))

    protected def current(j:Json) = getValue(j, prop.path.segments).flatMap(prop.seqPattern.unapply).getOrElse(Seq.empty)
  }

  implicit class MaybeArrayLens[T](val prop: \:?[T]) extends AnyVal {
    def at(index:Int) = ArrayElement[T](index, prop.path)(prop.elementPattern)

    def head = ArrayElement[T](0, prop.path)(prop.elementPattern)

    def append =
      (value:T) => (j:Json) =>
        setValue(Some(j), prop.path.segments, prop.seqPattern.apply(current(j) :+ prop.elementPattern(value)))

    def prepend =
      (value:T) => (j:Json) =>
        prop.seqPattern.apply(prop.elementPattern(value) +: current(j))

    protected def current(j:Json) = getValue(j, prop.path.segments).flatMap(prop.seqPattern.unapply).getOrElse(Seq.empty)
  }
  case class ArrayElement[T](index:Int, arrayPath:Path)(implicit val pattern:Pattern[T]) extends Property[T]  {
    def key = index.toString
    def validator: Validator[T] = EmptyValidator
    def path = arrayPath \ index
  }

  protected def setValue(target:Option[Json], segments:Segments, value:Json, insert:Boolean = false):Json =
    (segments, target) match {
      case (Left(key) +: tail, Some(JObject(obj))) =>
        JObject(obj + (key -> setValue(obj.get(key), tail, value)))
      case (Left(key) +: tail, _) =>
        JObject(key -> setValue(None, tail, value))
      case (Right(index) +: tail, Some(JArray(array))) =>
        val (left, right) = array.splitAt(index)
        if (left.size < index)
          JArray(left.padTo(index, JNull) :+ setValue(None, tail, value))
        else
          JArray((left :+ setValue(right.headOption, tail, value)) ++ right.tail)
      case (Right(index) +: tail, _) =>
        JArray(Seq.fill(index)(JNull) :+ setValue(None, tail, value))
      case _ =>
        value
    }

  protected def insertValue(target:Option[Json], segments:Segments, value:Json):Json =
    (segments, target) match {
      case (Left(key) +: tail, Some(JObject(obj))) =>
        JObject(obj + (key -> setValue(obj.get(key), tail, value)))
      case (Left(key) +: tail, _) =>
        JObject(key -> setValue(None, tail, value))
      case (Right(index) +: tail, Some(JArray(array))) =>
        val (left, right) = array.splitAt(index)
        if (left.size < index)
          JArray(left.padTo(index, JNull) :+ setValue(None, tail, value))
        else if (tail.isEmpty)
          JArray((left :+ value) ++ right)
        else
          JArray((left :+ setValue(right.headOption, tail, value)) ++ right.tail)
      case (Right(index) +: tail, _) =>
        JArray(Seq.fill(index)(JNull) :+ setValue(None, tail, value))
      case _ =>
        value
    }

  protected def dropValue(target:Json, segments:Segments):Json =
    (segments, target) match {
      case (Left(key) +: Vector(), JObject(obj)) =>
        JObject(obj - key)
      case (Left(key) +: tail, JObject(obj)) =>
        JObject(obj.get(key).fold(obj)(v => obj + (key -> dropValue(v, tail))))
      case (Right(index) +: Vector(), JArray(seq)) if index < seq.length =>
        val (left, right) = seq.splitAt(index)
        JArray(left ++ right.drop(1))
      case (Right(index) +: tail, JArray(seq)) if index < seq.length =>
        val (left, right) = seq.splitAt(index)
        JArray((left :+ dropValue(right.head, tail)) ++ right.drop(1))
      case _ =>
        target
    }
}
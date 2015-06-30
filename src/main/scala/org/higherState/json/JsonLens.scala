package org.higherState.json


object JsonLens {
  import JsonPath._
  import JsonFunctions._

  implicit class LensCombinator(val f: Json => Json) extends AnyVal {
    def ~(f2: Json => Json): Json => Json =
      (j: Json) => f2(f(j))
  }

  implicit class ValueContractExt[T](val c:ValueContract[T]) extends AnyVal {
    def $create(value:T):Json =
      c.pattern.apply(value)
  }

  implicit class ContractExt[T <: BaseContract](val c:T) extends AnyVal {
    def $create(f:c.type => Json => Json):JObject =
      f(c)(JObject.empty).asInstanceOf[JObject]
  }

  implicit class ContractTypeExt[T <: ContractType](val c:T) extends AnyVal {
    def $create(f:c.type => Json => Json):JObject =
      f(c)(JObject(Map(c.key -> c.matcher.default))).asInstanceOf[JObject]
    def $create() = JObject(Map(c.key -> c.matcher.default))
  }

  implicit class ValueLens[T](val prop: Property[T]) extends AnyVal {

    def $get(j:Json):Option[T] =
      getValue(j, prop.absolutePath.segments).flatMap(prop.pattern.unapply)
    def $set =
      (value:T) => (j:Json) => setValue(Some(j), prop.absolutePath.segments, prop.pattern(value))
    //applies set if value is nonEmpty, does not drop on empty
    def $modify =
      (func:T => T) => (j:Json) =>
        $get(j).fold[Json](j)(v => setValue(Some(j), prop.absolutePath.segments, prop.pattern(func(v))))
    def $copy =
      (p:Property[T]) => (j:Json) => {
        getValue(j, prop.absolutePath.segments) match {
          case None =>
          case Some(value) =>
            insertValue(Some(j), p.absolutePath.segments, value)
        }
      }
  }

  implicit class MaybeValueLens[T](val prop: Property[Option[T]]) extends AnyVal {

    def $maybeSet =
      (value:Option[T]) => (j:Json) =>
        value.fold(j){v =>
          setValue(Some(j), prop.absolutePath.segments, prop.pattern(Some(v)))
        }
    def $modifyOrDrop =
      (func:Option[T] => Option[T]) => (j:Json) => {
        getValue(j, prop.absolutePath.segments).flatMap(prop.pattern.unapply).flatten.fold{
          dropValue(j, prop.absolutePath.segments)
        } { v =>
          setValue(Some(j), prop.absolutePath.segments, prop.pattern(func(Some(v))))
        }
      }
    //applies set if value is nonEmpty, does not drop on empty
    def $drop =
      (j:Json) => dropValue(j, prop.absolutePath.segments)

    def $setOrDrop =
      (value:Option[Option[T]]) => (j:Json) => value.fold(dropValue(j, prop.absolutePath.segments)){v =>
        setValue(Some(j), prop.absolutePath.segments, prop.pattern(v))
      }
  }

  implicit class JsonLens[T](val json:Json) extends AnyVal {
    def select(properties:Property[_]*):Json = {
      properties.foldLeft(JObject.empty.asInstanceOf[Json]) { (j, p) =>
        getValue(json, p.absolutePath.segments).fold(j){v =>
          setValue(Some(j), p.absolutePath.segments, v)
        }
      }
    }
    def exclude(properties:Property[_]*):Json = {
      properties.foldLeft(json){ (j, p) =>
        dropValue(j, p.absolutePath.segments)
      }
    }
    def append(params:(String, Json)*):Json = json match {
      case JObject(value) => JObject(value ++ params)
      case _ => json
    }
    def concat(value:Json):Json = json -> value match {
      case (JObject(c), JObject(d)) => JObject(c ++ d)
      case (JArray(c), JArray(d)) => JArray(c ++ d)
      case (JArray(c), d) => JArray(c :+ d)
      case (c, d) => JArray(Seq(c, d))
    }

    def delta(delta:Json):Json =
      applyDelta(json, delta)
  }

  implicit class ArrayLens[T](val prop: \:[T]) extends AnyVal {
    def $at(index:Int) = ArrayElement[T](index, prop.absolutePath)(prop.elementPattern)

    def $head = ArrayElement[T](0, prop.absolutePath)(prop.elementPattern)

    def $append =
      (value:T) => (j:Json) =>
        setValue(Some(j), prop.absolutePath.segments, prop.seqPattern.apply(current(j) :+ prop.elementPattern(value)))

    def $prepend =
      (value:T) => (j:Json) =>
        prop.seqPattern.apply(prop.elementPattern(value) +: current(j))

    protected def current(j:Json) = getValue(j, prop.absolutePath.segments).flatMap(prop.seqPattern.unapply).getOrElse(Seq.empty)
  }

  implicit class MaybeArrayLens[T](val prop: \:?[T]) extends AnyVal {
    def $at(index:Int) = ArrayElement[T](index, prop.absolutePath)(prop.elementPattern)

    def $head = ArrayElement[T](0, prop.absolutePath)(prop.elementPattern)

    def $append =
      (value:T) => (j:Json) =>
        setValue(Some(j), prop.absolutePath.segments, prop.seqPattern.apply(current(j) :+ prop.elementPattern(value)))

    def $prepend =
      (value:T) => (j:Json) =>
        prop.seqPattern.apply(prop.elementPattern(value) +: current(j))

    protected def current(j:Json) = getValue(j, prop.absolutePath.segments).flatMap(prop.seqPattern.unapply).getOrElse(Seq.empty)
  }

  case class ArrayElement[T](index:Int, arrayPath:Path)(implicit val pattern:Pattern[T]) extends Property[T]  {

    def relativePath: Path = Path(index)
    def validator: Validator[T] = EmptyValidator
    def absolutePath = arrayPath \ index
  }


}
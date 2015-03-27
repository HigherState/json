package org.higherState.json

import scalaz._
import Scalaz._

object JsonQuery {
  import JsonConstructor._
  import JsonLens._

  implicit class JsonQueryExt(val json:Json) extends AnyVal {
    def isMatch(value:Json) = json.exists {
      case j:JObject => apply(Some(value), j)
      case v => v == value
    }
    def &&(d:Json) =
      json.collect {
        case JObject(m) if m.contains("$or") && d.exists{ case JObject(m2) if m2.contains("$or") => true} =>
          JObject("$and" -> JArray(json, d))
        case JObject(m) if m.contains("$and") =>
          d.collect {
            case JObject(m2) if m2.contains("$and") =>
              JObject("$and" -> m("$and").concat(m2("$and")))
          }.getOrElse(JObject("$and" -> m("$and").concat(d)))
      }.getOrElse(JsonFunctions.applyDelta(json, d))


    def ||(d:Json) = json match {
      case JObject(seq) =>
        seq.get("$or").collect {
          case JArray(seq) => JObject("$or" -> JArray(seq :+ d))
        }.getOrElse(JObject("$or" -> JArray(json, d)))
    }
  }

  implicit class ValueQuery[T](val prop: Property[T]) extends AnyVal {
    def $eq(value:T) = nest(prop.pattern.apply(value))
    def $ne(value:T) = nest(JObject("$ne" -> prop.pattern.apply(value)))
    def $in(values:T*) = nest(JObject("$in" -> JArray(values.map(prop.pattern.apply))))
    def $nin(values:T*) = nest(JObject("$nin" -> JArray(values.map(prop.pattern.apply))))
    def $exists(value:Boolean) = nest(JObject("$exists" -> value.j))

    private def nest(obj:Json) =
      pathToObject(prop.absolutePath.segments, obj)
  }

  implicit class NumericQuery[T >: JNumeric](val prop: Property[T]) extends AnyVal {

    def $lt(value:Double) = nest(JObject("$lt" -> value.j))
    def $lt(value:Long) = nest(JObject("$lt" -> value.j))

    def $gt(value:Double) = nest(JObject("$gt" -> value.j))
    def $gt(value:Long) = nest(JObject("$gt" -> value.j))

    def $lte(value:Double) = nest(JObject("$lt" -> value.j))
    def $lte(value:Long) = nest(JObject("$lt" -> value.j))

    def $gte(value:Double) = nest(JObject("$gt" -> value.j))
    def $gte(value:Long) = nest(JObject("$gt" -> value.j))

    private def nest(obj:JObject) =
      pathToObject(prop.absolutePath.segments, obj)
  }

  def apply(value:Option[Json], query:JObject):Boolean = {
    query.value.forall{
      case ("$and", JArray(values)) =>
        values.collect{case j:JObject => j}.forall(apply(value, _))
      case ("$or", JArray(values)) =>
        values.collect{case j:JObject => j}.exists(apply(value, _))
      case ("$eq", v) =>
        value.exists(_ == v)
      case ("$ne", v) =>
        !value.exists(_ == v) //neq doesnt require existence, as per mongodb
      case ("$lt", v) =>
        value.exists(order.lift(_, v).exists(_ == Ordering.LT))
      case ("$gt", v) =>
        value.exists(order.lift(_, v).exists(_ == Ordering.GT))
      case ("$lte", v) =>
        value.exists(order.lift(_, v).exists(r => r == Ordering.LT || r == Ordering.EQ))
      case ("$gte", v) =>
        value.exists(order.lift(_, v).exists(r => r == Ordering.GT || r == Ordering.EQ))
      case ("$in", JArray(values)) =>
        value.exists(j => values.exists(order.lift(_, j) == Ordering.EQ))
      case ("$nin", JArray(values)) =>
        !value.exists(j => values.exists(order.lift(_, j) == Ordering.EQ)) //nin doesnt require existence, as per mongodb
      case ("$exists", JBool(v)) =>
        value.isDefined == v
      case (key, v:JObject) =>
        value match {
          case Some(JObject(map)) =>
            apply(map.get(key), v)
          case None =>
            apply(None, v)
        }
      case (key, j) =>
        value.exists {
          case JObject(map) if map.contains(key) =>
            map(key) == j
        }
    }
  }

  private def pathToObject(path:Segments,obj:Json):Json= {
    path match {
      case Seq() => obj
      case head :+ Left(tail) => pathToObject(head, JObject(tail -> obj))
    }
  }
  private def stripPath(path:Segments, obj:JObject):JObject = {
    path match {
      case Seq() => obj
      case Left(head) +: tail => stripPath(tail, obj.value(head).asInstanceOf[JObject])
    }
  }

  def order:PartialFunction[(Json, Json), Ordering] = {
    case (JDouble(x), JDouble(y)) => x ?|? y
    case (JLong(x), JLong(y)) => x ?|? y
    case (JString(x), JString(y)) => x ?|? y
    case (JBool(x), JBool(y)) => x ?|? y
    case (JDouble(x), JLong(y)) => x ?|? y
    case (JLong(x), JDouble(y)) => x.toDouble ?|? y
  }
}
package org.higherState.json

import scalaz._
import Scalaz._

object JsonQuery {
  import JsonConstructor._
  import JsonLens._

  implicit class JsonQueryExt(val json:Json) extends AnyVal {
    def find(query:Json) = query.exists {
      case j:JObject => isMatch(json, j)
      case v => v == json
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
    def $neq(value:T) = nest(JObject("$ne" -> prop.pattern.apply(value)))
    def $in(values:T*) = nest(JObject("$in" -> JArray(values.map(prop.pattern.apply))))
    def $nin(values:T*) = nest(JObject("$nin" -> JArray(values.map(prop.pattern.apply))))
    def $exists(value:Boolean) = nest(JObject("$exists" -> value.j))

    private def nest(obj:Json) =
      pathToObject(prop.absolutePath.segments, obj)
  }

  implicit class NumericQuery[T >: JsonValidation.Numeric](val prop: Property[T]) extends AnyVal {

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

  def isMatch(value:Json, query:JObject):Boolean = {
    query.value.forall{
      case ("$and", JArray(values)) =>
        values.collect{case j:JObject => j}.forall(isMatch(value, _))
      case ("$or", JArray(values)) =>
        values.collect{case j:JObject => j}.exists(isMatch(value, _))
      case ("$eq", v) =>
        v == value
      case ("$lt", v) =>
        order.lift(value, v).exists(_ == Ordering.LT)
      case ("$gt", v) =>
        order.lift(value, v).exists(_ == Ordering.GT)
      case ("$lte", v) =>
        order.lift(value, v).exists(r => r == Ordering.LT || r == Ordering.EQ)
      case ("$gte", v) =>
        order.lift(value, v).exists(r => r == Ordering.GT || r == Ordering.EQ)
      case ("$in", JArray(values)) =>
        values.exists(order.lift(_,value) == Ordering.EQ)
      case (key, v:JObject) =>
        value.exists {
          case JObject(map) if map.contains(key) =>
            isMatch(map(key), v)
        }
      case (key, j) => value == j
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
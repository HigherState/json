package org.higherState

import org.joda.time.DateTime

package object json {

  type JMap = Map[String, JType]

  object && {
    def unapply[A](a: A) = Some((a, a))
  }

  implicit class JsonOptionExt[A <: JType](val self:Option[A]) extends AnyVal  {
    def \ (path:String):Option[JType] =
      self.flatMap(_ \ path)

    //cannot use collect for some reason here

    def getLong:Option[Long] =
      self.flatMap {
        case JLong(i) => Some(i)
        case _        => None
      }

    def getDouble:Option[Double] =
      self.flatMap {
        case JDouble(i) => Some(i)
        case JLong(i) => Some(i)
        case _          => None
      }


    def getString:Option[String] =
      self.flatMap {
        case JText(i) => Some(i)
        case _        => None
      }

    def getBool:Option[Boolean] =
      self.flatMap {
        case JBool(i) => Some(i)
        case _        => None
      }

    def getDateTime:Option[DateTime] =
      self.flatMap {
        case JDateTime(i) => Some(i)
        case _            => None
      }

    def getUUID:Option[java.util.UUID] =
      self.flatMap {
        case JUUID(i) => Some(i)
        case _        => None
      }

    def getArray:Option[Seq[JType]] =
      self.flatMap {
        case JArray(i) => Some(i)
        case _         => None
      }

    def asJObject:Option[JObject] =
      self.flatMap {
        case j:JObject => Some(j)
        case _ => None
      }

    def asJVal:Option[JVal] =
      self.flatMap {
        case j:JVal => Some(j)
        case _ => None
      }

    def asJText:Option[JText] =
      self.flatMap {
        case j:JText => Some(j)
        case _        => None
      }

    def asJUUID:Option[JUUID] =
      self.flatMap {
        case j:JUUID => Some(j)
        case _ => None
      }

    def asJDateTime:Option[JDateTime] =
      self.flatMap {
        case j:JDateTime => Some(j)
        case _ => None
      }

    def asJArray:Option[JArray] =
      self.flatMap {
        case j:JArray => Some(j)
        case _ => None
      }
  }

  implicit class UUIDtoJ(val value:java.util.UUID) extends AnyVal {
    def j = JUUID(value)
  }
  implicit class StringtoJ(val value:String) extends AnyVal {
    def j = JText(value)
    def \ = Path(Vector(value))
  }
  implicit class MaptoJ(val value:Map[String,JType]) extends AnyVal {
    def j = JObject(value)
  }
  implicit class TupleItertoJ(val value:TraversableOnce[(String,JType)]) extends AnyVal {
    def j = JObject(value.toMap)
  }
  implicit class InttoJ(val value:Int) extends AnyVal {
    def j = JLong(value)
  }
  implicit class LongtoJ(val value:Long) extends AnyVal {
    def j = JLong(value)
  }
  implicit class FloattoJ(val value:Float) extends AnyVal {
    def j = JDouble(value)
  }
  implicit class DoubletoJ(val value:Double) extends AnyVal {
    def j = JDouble(value)
  }
  implicit class BooltoJ(val value:Boolean) extends AnyVal {
    def j = JBool(value)
  }
  implicit class DateTimetoJ(val value:DateTime) extends AnyVal {
    def j = JDateTime(value)
  }
  implicit class SeqtoJ(val value:Seq[JType]) extends AnyVal {
    def j = JArray(value)
  }
  implicit class ItertoJ(val value:TraversableOnce[JType]) extends AnyVal {
    def j = JIter(value)
  }

  val JTrue = JBool(value = true)
  val JFalse = JBool(value = false)
}
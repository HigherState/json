package org.higherState.json

import java.util.UUID
import org.joda.time.DateTime

sealed trait JVal extends Any with JType
object JVal {
  def unapply(j:JType) =
    j match {
      case j:JVal => Some(j.value)
      case _ => None
    }
}

trait JUndefined extends JVal {
  def value = this
}
object JUndefined extends JUndefined

trait JNull extends JVal {
  def value = null
}
object JNull extends JNull

sealed trait JNumber extends Any with JVal
object JNumber {
  def unapply(j:JType) =
    j.value match {
      case n:Number =>
        Some(n)
      case _ => None
    }
}
case class JLong(value:Long) extends AnyVal with JNumber
case class JDouble(value:Double) extends AnyVal with JNumber

trait JText extends Any with JVal {
  def value:String
}
private case class JTextImpl(value:String) extends AnyVal with JText
object JText {
  val empty:JText = JTextImpl("")

  def apply(text:String):JText = JTextImpl(text)

  def unapply(j:JText) =
    Some(j.value)
}

case class JBool(value:Boolean) extends AnyVal with JVal
case class JDateTime(value:DateTime) extends AnyVal with JVal

trait JUUID extends Any with JVal {
  def value:UUID
}
private case class JUUIDImpl(value:UUID) extends AnyVal with JUUID
object JUUID {
  def apply(u:UUID):JUUID =
    JUUIDImpl(u)
  def apply(major:Long, minor:Long):JUUID =
    JUUIDImpl(new UUID(major, minor))
  def randomUUID :JUUID =
    JUUIDImpl(UUID.randomUUID())
  val empty:JUUID  =
    JUUIDImpl(new UUID(0,0))
  def unapply(j:JUUID) =
    Some(j.value)
  def fromString(id:String):Option[JUUID] =
    try { Some(JUUID(UUID.fromString(id)))}
    catch {
      case ex:IllegalArgumentException => None
    }
}






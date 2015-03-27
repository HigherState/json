package org.higherState

import scalaz.ValidationNel

package object json {

  type JMap = Map[String, Json]
  type Segments = Vector[Either[String, Int]]
  type JValid = ValidationNel[(String, Path), Json]

  type JNumeric = Long with Int with Float with Double with Option[Long] with Option[Int] with Option[Float] with Option[Double]
  type JLength = String with Seq[Nothing]
  type JOptionable[T] = T with Option[T]

  object && {
    def unapply[A](a: A) = Some((a, a))
  }

  val JTrue = JBool(true)
  val JFalse = JBool(false)

  implicit def stringToPath(s:String) = Path(s)
}


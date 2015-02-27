package org.higherState

import scalaz.ValidationNel

package object json {

  type JMap = Map[String, Json]
  type Segments = Vector[Either[String, Int]]
  type ValidationFailure = (String, Path)
  type JValid = ValidationNel[ValidationFailure, Json]

  object && {
    def unapply[A](a: A) = Some((a, a))
  }

  val JTrue = JBool(true)
  val JFalse = JBool(false)

  implicit def stringToPath(s:String) = Path(s)
}


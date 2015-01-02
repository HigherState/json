package org.higherState

package object json {

  type JMap = Map[String, Json]
  type Segments = Vector[Either[String, Int]]

  object && {
    def unapply[A](a: A) = Some((a, a))
  }
}
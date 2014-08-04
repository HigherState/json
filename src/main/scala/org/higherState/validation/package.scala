package org.higherState

import scalaz.ValidationNel

package object validation {

  type Valid[+T] = ValidationNel[ValidationFailure, T]
}

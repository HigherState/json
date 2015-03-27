package org.higherState.json

import scalaz.NonEmptyList
import org.higherState.json.JsonFunctions._
import scalaz.Failure
import scalaz.Success

object JsonValidation extends ValidationPropertyCache {

  implicit class BaseContractValidation(val contract:BaseContract) extends AnyVal {
    def $validate(newContent:Json):JValid =
      $validate(newContent, None, Path.empty) match {
        case Nil => Success(newContent)
        case failure +: failures => Failure(NonEmptyList(failure, failures:_*))
      }

    def $validate(deltaContent:Json, currentState:Json):JValid =
      $validate(deltaContent, Some(currentState), Path.empty) match {
        case Nil => Success(deltaContent)
        case failure +: failures => Failure(NonEmptyList(failure, failures:_*))
      }
    //TODO better approach here
    def $validate(value: Json, currentState: Option[Json], path:Path): Seq[(String, Path)] =
      getProperties(contract).flatMap{p =>
        val v = JsonPath.getValue(value, p.relativePath.segments)
        val c = currentState.flatMap(JsonPath.getValue(_, p.relativePath.segments))
        p.$validate(v, c, path ++ p.relativePath)
      }

    def $sanitize(json:Json):Json = {
      getInternal(contract).foldLeft(json){ (j, p) =>
        dropValue(j, p.absolutePath.segments)
      }
    }
  }

  implicit class PropertyValidation[T](val prop:Property[T]) extends AnyVal {
    def $validate(value: Option[Json], currentState: Option[Json], path:Path): Seq[(String, Path)] =
      ((value, currentState, prop) match {
        case (None, None, p: Expected[_]) =>
          Seq("Value required." -> path)
        case (Some(v), c, _) if prop.pattern.unapply(v).isEmpty =>
          Seq(s"Unexpected type '${v.getClass.getSimpleName}'." -> path)
        case (Some(v), c, b:BaseContract) =>
          new BaseContractValidation(b).$validate(v, c, path)
        case _ =>
          Seq.empty
      }) ++ prop.validator.validate(value, currentState, path)
  }
}

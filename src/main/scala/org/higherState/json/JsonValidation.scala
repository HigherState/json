package org.higherState.json

import scalaz.NonEmptyList
import org.higherState.json.JsonFunctions._
import scalaz.Failure
import scalaz.Success

object JsonValidation extends ValidationPropertyCache {

  //TODO: test validation of nested Contracts with forall/values
  implicit class ValueContractValidation[T](val contract:ValueContract[T]) extends AnyVal {
    def $validate(newContent:Json):JValid =
      seqToJValid($validate(newContent, None, Path.empty), newContent)

    def $validate(deltaContent:Json, currentState:Json):JValid =
      seqToJValid($validate(deltaContent, Some(currentState), Path.empty), deltaContent)
    //TODO better approach here
    def $validate(deltaContent:Json, currentState:Option[Json]):JValid =
      seqToJValid($validate(deltaContent, currentState, Path.empty), deltaContent)

    def $validate(value: Json, currentState: Option[Json], path:Path): Seq[(String, Path)] =
      contract.validator.validate(Some(value), currentState, path)

    def $sanitize(json:Json):Json = ???
  }

  implicit class BaseContractValidation(val contract:BaseContract) extends AnyVal {
    def $validate(newContent:Json):JValid =
      seqToJValid($validate(newContent, None, Path.empty), newContent)

    def $validate(deltaContent:Json, currentState:Json):JValid =
      seqToJValid($validate(deltaContent, Some(currentState), Path.empty), deltaContent)

    def $validate(deltaContent:Json, currentState:Option[Json]):JValid =
      seqToJValid($validate(deltaContent, currentState, Path.empty), deltaContent)
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

  private def seqToJValid(seq:Seq[(String, Path)], json:Json) =
    seq match {
      case Nil => Success(json)
      case failure +: failures => Failure(NonEmptyList(failure, failures:_*))
    }
}

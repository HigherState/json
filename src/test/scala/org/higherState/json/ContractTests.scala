package org.higherState.json

import org.scalatest.{Matchers, FunSuite}
import org.scalatest.concurrent.ScalaFutures
import Validators._

class ContractTests extends FunSuite with Matchers with ScalaFutures {

  test("Contract extractor test") {

    val document = JObject("Id" -> "1231-123142-134134-241224".j, "age" -> 123.j, "metadata" -> JObject("name" -> "Bob".j))

    document match {
      case Document.age(age) && Document.metadata.name(JText("Bob")) =>
        println(age)
      case _ =>
        println("No match")
    }

    println(Document.validate(New, Some(document)))
  }

}

object Document extends Document(None)
case class Document(parent:Parent = None) extends JObjectContract {

  val age = property[JLong]("age") being Required and > (0)

  val metadata = property(Metadata(), "metadata")
}

object Metadata extends Metadata(None)
case class Metadata(parent:Parent = None) extends JObjectContract {

  val name = property[JText]("name") being Immutable
}
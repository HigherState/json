package org.higherState.json

import org.scalatest.{Matchers, FunSuite}
import org.scalatest.concurrent.ScalaFutures

class ContractTests extends FunSuite with Matchers with ScalaFutures {
  import Validators._

  trait Created extends SubContract {
    val user = new Property[String]("name")
    val time = new Property[Long]("time")
  }

  trait Metadata extends SubContract {
    val name = new Property[String]("name", required && notNull)
    val created = new Property("created")
  }

  object Document extends Contract with Created {
    val id = new Property[String]("Id", immutable)
    val age = new Property[JLong]("age", Validators.< (125) && Validators.>= (0))
    val metadata = new Property("metadata") with Metadata
  }

  test("Contract extractor test") {

    val document = JObject("Id" -> "1231-123142-134134-241224".j, "age" -> 123.j, "metadata" -> JObject())

    document match {
      case Document.age(age) && Document.metadata.name(bob) && Document.user.?(user) =>
        println(age, bob, user)
      case Document.age(age) && Document.user.?(user) =>
        println(age, user)
      case _ =>
        println("No match")
    }

    println(Document.validate(document, Some(JObject("Id" -> "1231-123142-134134-241225".j, "metadata" -> JObject("name" -> "bob".j)))))
  }

}
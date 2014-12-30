package org.higherState.json

import org.scalatest.{Matchers, FunSuite}
import org.scalatest.concurrent.ScalaFutures
import shapeless.HNil

class ContractTests extends FunSuite with Matchers with ScalaFutures {
  import Validators._
  import DefaultPatterns._

  trait Created extends SubContract {
    val user = new Property[String]("user")
    val time = new Property[Long]("time")
  }

  trait Metadata extends SubContract {
    val name = new Property[String]("name", required && notNull)
    val created = new Property[JMap]("created") with Created
  }

  object Document extends Contract with Created {
    val id = new Property[String]("Id", immutable)
    val age = new Property[Long]("age", Validators.< (125) && Validators.>= (0))
    val metadata = new Property[JMap]("metadata") with Metadata
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

  object Collection extends Contract {
    val coll = new Property[Seq[String]]("coll", as(Document))
    val tupl = new Property[(Long, String)]("tupl")
  }

  test("Collection") {
    val c = JObject("coll" -> JArray("one".j, "two".j, 2.j), "tupl" -> JArray(4.j, "four".j))

    c match {
      case Collection.coll(v) =>
        println(v)
    }
    c match {
      case Collection.tupl((l, r)) =>
        println(l, r)
    }

    println(Collection.validate(c))
  }

  test("Single setting and modifying") {
    val document = JObject("Id" -> "1231-123142-134134-241224".j, "age" -> 123.j, "metadata" -> JObject())
    println(Document.metadata.name.set("John")(document))
    println(Document.age.modify(_ + 1)(document))
  }

  test("Composition of setters") {
    import Compositor._
    val document = JObject("Id" -> "1231-123142-134134-241224".j, "age" -> 123.j, "metadata" -> JObject("name" -> "John".j))

    val modify = Document{d =>
      d.age.modify(_ + 1) ~
      d.metadata{m =>
        m.name.move(m.created.user)
      } ~
      d.id.clear
    }
    println(modify(document))
  }

}
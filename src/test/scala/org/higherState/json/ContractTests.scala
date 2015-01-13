package org.higherState.json

import java.util.UUID

import org.scalatest.{Matchers, FunSuite}
import org.scalatest.concurrent.ScalaFutures

class ContractTests extends FunSuite with Matchers with ScalaFutures {
  import Validation._
  import DefaultPatterns._
  import JsonConstructor._

  trait Created extends SubContract {
    val user = Value[String]("user")
    val time = Value[Long]("time")
  }

  trait Metadata extends SubContract {
    val name = Value[String]("name", required && notNull)
    val created = new Object("created") with Created
  }

  object Document extends Contract with Created {
    val id = Value[UUID]("Id", immutable && required)
    val age = Value[Long]("age", Validation.< (125) && Validation.>= (0))
    val metadata = new Object("metadata") with Metadata
  }

  test("Contract extractor test") {

    val document = Map("Id" -> UUID.randomUUID().toString.j, "age" -> 123.j, "metadata" -> JObject.empty).j

    document match {
      case Document.age(age) && Document.metadata.name(bob) && Document.user.?(user) =>
        println(age, bob, user)
      case Document.age(age) && Document.user.?(user) =>
        println(age, user)
      case _ =>
        println("No match")
    }

    println(Document.validate(document))
    println(Document.validate(document, Some(JObject("Id" -> UUID.randomUUID().toString.j, "metadata" -> JObject("name" -> "bob".j)))))
    println(Document.schema)
  }





  test("Single setting and modifying") {
    val document = Map("Id" -> UUID.randomUUID().toString.j, "age" -> 123.j, "metadata" -> JObject()).j
    println(Document.metadata.name.set("John")(document))
    println(Document.age.modify(_ + 1)(document))
  }






  test("Composition of setters") {
    import Compositor._
    val document = Map("Id" -> UUID.randomUUID().toString.j, "age" -> 123.j, "metadata" -> Map("name" -> "John".j).j).j

    val modify = Document{d =>
      d.age.modify(_ + 1) ~
        d.metadata{m =>
          m.name.move(m.created.user)
        } ~
        d.id.drop
    }
    println(modify(document))
  }







  trait Collection extends Contract {
    val coll = Array[String]("coll")
    val tupl = Value[(Long, String)]("tupl")
    val obj = Array[Json]("obj", forall(Document))
  }
  object Collection extends Contract with Collection

  test("Collection") {
    import Compositor._
    val c = JObject("coll" -> Seq("one".j, "two".j, 2.j).j,
      "tupl" -> JArray(4.j, "four".j),
      "obj" -> JArray(Map("Id" -> UUID.randomUUID().toString.j).j))

    c match {
      case Collection.coll(v) =>
        println(v)
    }
    c match {
      case Collection.tupl((l, r)) =>
        println(l, r)
    }
    c match {
      case Collection.obj(Document.id(id) +: Nil) =>
        println(id)
    }
    println(Collection.coll{ c =>
      c.append("three") ~
      c.at(0).modify(_ + "_") ~
      c.at(6).set("six") ~
      c.at(2).move(c.at(4))
    }(c))

    println {
      Collection.obj { c =>
        c.append(
          Document{ d =>
            d.id.set(UUID.randomUUID()) ~
            d.age.set(34)
          }(JObject.empty)
        )
      }(c)
    }
    println(Collection.validate(c))
    println(Collection.schema)
  }

}
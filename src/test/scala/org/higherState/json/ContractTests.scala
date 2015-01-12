package org.higherState.json

import org.scalatest.{Matchers, FunSuite}
import org.scalatest.concurrent.ScalaFutures

class ContractTests extends FunSuite with Matchers with ScalaFutures {
  import Validation._
  import DefaultPatterns._
  import JsonConstructor._

  trait Created extends Contract {
    val user = Value[String]("user")
    val time = Value[Long]("time")
  }

  trait Metadata extends Contract {
    val name = Value[String]("name", required && notNull)
    val created = new Object("created") with Created
  }

  trait Document extends Contract with Created {
    val id = Value[String]("Id", immutable && required)
    val age = Value[Long]("age", Validation.< (125) && Validation.>= (0))
    val metadata = new Object("metadata") with Metadata
  }

  object Document extends Extractor with Document

  test("Contract extractor test") {

    val document = Map("Id" -> "1231-123142-134134-241224".j, "age" -> 123.j, "metadata" -> Map.empty[String, Json].j).j

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

  trait Collection extends Contract {
    val coll = Array[String]("coll")
    val tupl = Value[(Long, String)]("tupl")
    val obj = Array[Json]("objs", forall(Document))
  }
  object Collection extends Extractor with Collection

  test("Collection") {
    import Compositor._
    val c = JObject("coll" -> Seq("one".j, "two".j, 2.j).j, "tupl" -> JArray(4.j, "four".j), "obj" -> JArray(Map("Id" -> "123-412312312-123123".j).j))

    c match {
      case Collection.coll(v) =>
        println(v)
    }
    c match {
      case Collection.tupl((l, r)) =>
        println(l, r)
    }
    println(Collection.coll{ c =>
      c.append("three") ~
      c.at(0).modify(_ + "_") ~
      c.at(6).set("six") ~
      c.at(2).move(c.at(4))
    }(c))

    println(Collection.validate(c))
    println(Collection.getSchema)
  }

  test("Single setting and modifying") {
    val document = Map("Id" -> "1231-123142-134134-241224".j, "age" -> 123.j, "metadata" -> JObject()).j
    println(Document.metadata.name.set("John")(document))
    println(Document.age.modify(_ + 1)(document))
  }

  test("Composition of setters") {
    import Compositor._
    val document = Map("Id" -> "1231-123142-134134-241224".j, "age" -> 123.j, "metadata" -> Map("name" -> "John".j).j).j

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
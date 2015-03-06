package org.higherState.json

import java.util.UUID

import org.scalatest.{Matchers, FunSuite}
import org.scalatest.concurrent.ScalaFutures

class ContractTests extends FunSuite with Matchers with ScalaFutures {

  import DefaultPatterns._
  import JsonConstructor._
  import JsonValidation._
  import JsonMatchers._

  trait Created extends SubContract {
    val user = \[String]("user", nonEmptyOrWhiteSpace)
    val time = \[Long]("time")
  }

  trait Metadata extends SubContract {
    val name = \[String]("name", nonEmptyOrWhiteSpace)
    val created = new \\?("created", reserved) with Created
  }

  object Document extends Contract {
    val id = \[UUID]("Id")
    val age = \[Long]("age", immutable && JsonValidation.>=(0) && JsonValidation.<=(100))
    val default = \![Boolean]("default", false, notNull)
    val metadata = new \\("metadata") with Metadata
    val phone = \?[Long]("phone", reserved && JsonValidation.>=(0))
  }

  object TypeTest extends ContractType("type", "Test") {
    val id = \[UUID]("Id")
  }

  object KeyMatchTest extends ContractType("type") {
    val id = \[UUID]("Id")
  }

  object Nested extends Contract {
    val maybeObj = new \\?("maybeObj") {
      val messages = \:?[Seq[Json]]("messages")
    }
  }

  test("Contract extractor test") {
    import JsonLens._

    val document = Map("Id" -> UUID.randomUUID().toString.j, "age" -> 123.j, "metadata" -> JObject.empty).j

    document match {
      case Document.age(age) && Document.metadata.name(bob) && Document.default(default) =>
        println(age, bob, default)
      case Document.age(age) && Document.phone(maybe) && Document.default(false) =>
        println(age, maybe)
      case _ =>
        println("No match")
    }

    println(Document.validate(document))
    println(Document.validate(document, JObject("Id" -> UUID.randomUUID().toString.j, "age" -> 223.j)))
//    println(Document.schema)

    println(document.select(Document.age, Document.metadata.name))
    println(document.exclude(Document.id))

    println(document.append("temp" -> 1.j))

    println(document.concat(JObject("temp" -> 1.j)))
  }

  test("Validate") {
    println(Document.validate(Map("Id" -> "hi".j).j))
  }

  test("Contract type test") {
    import JsonLens._
    val document = Map("Id" -> UUID.randomUUID().toString.j, "type" -> "Test".j).j
    document match {
      case TypeTest(_) && KeyMatchTest(_) =>
        println("Matched")
    }

    println(TypeTest.create(_.id.set(UUID.randomUUID())))
  }

  test("test path") {
    Path.fromString("hi") should be (Path("hi"))
    Path.fromString("one\\two\\3\\four") should be (Path("one", "two", 3, "four"))
  }



  test("Single setting and modifying") {
    import JsonLens._
    val document = Map("Id" -> UUID.randomUUID().toString.j, "age" -> 123.j, "metadata" -> JObject()).j
    println(Document.metadata.name.set("John")(document))
    println(Document.age.modify(_ + 1)(document))
    println(Document.phone.modify(_.map(_ + 2372321).orElse(Some(12323)))(document))
  }






//  test("Composition of setters") {
//    import JsonLens._
//    val document = Map("Id" -> UUID.randomUUID().toString.j, "age" -> 123.j, "metadata" -> Map("name" -> "John".j).j).j
//
//    val modify = Document{d =>
//      d.age.modify(_ + 1) ~
//        d.metadata{m =>
//          m.name.move(m.created.user)
//        } ~
//        d.id.drop
//    }
//    println(modify(document))
//  }





  trait Collection extends Contract {
    val coll = \:[String]("coll")
    val tupl = \[(Long, String)]("tupl")
    val obj = \[Seq[Json]]("obj")
  }
  object Collection extends Contract with Collection

  test("Collection") {
    import JsonLens._

    val c = JObject("coll" -> Seq("one".j, "two".j, "three".j).j,
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
    println(Collection.coll.modify{ c =>
      "three" +: c
    }(c))

//    println(Collection.coll{ c =>
//      c.append("three") ~
//        c.at(0).modify(_ + "_") ~
//        c.at(6).set("six") ~
//        c.at(2).move(c.at(4))
//    }(c))

  }

}
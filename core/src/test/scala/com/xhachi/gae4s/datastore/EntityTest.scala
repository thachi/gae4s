package com.xhachi.gae4s.datastore

import java.util.Date

import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig
import com.xhachi.gae4s.tests.AppEngineTestSuite
import org.scalatest.FunSuite


class EntityTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  test("Entityが正しく構築できること") {
    val key: Key = Datastore.createKey("user", "key_name")
    val target = Entity(key, Seq(
      IndexedProperty("name", "Taro"),
      IndexedProperty("height", 123),
      IndexedProperty("deleted", true),
      CreationDateProperty(new Date)
    ))

    assert(target.properties.size == 4)
    assert(target.properties.map(_.name).contains("name"))

    val nameProperty: Option[Property[_]] = target.properties.find(_.name == "name")
    assert(nameProperty.isDefined)
    assert(nameProperty.get.name == "name")
    assert(nameProperty.get.value == "Taro")

    assert(target[String]("name") == "Taro")
    assert(target[Int]("height") == 123)
    assert(target[Boolean]("deleted") == true)
    assert(target[Date]("createdAt") != null)
  }

}



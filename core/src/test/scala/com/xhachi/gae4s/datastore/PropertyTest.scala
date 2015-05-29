package com.xhachi.gae4s.datastore

import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig
import com.xhachi.gae4s.tests.AppEngineTestSuite
import org.scalatest.FunSuite

class LongPropertyTest extends FunSuite {


  test("fromStorePropertyが正しいこと") {
    val property: Property[Long] = new LongProperty("name")

    assert(property.fromStoreProperty(1L) == 1L, "1Lが変換できる")
    assert(property.fromStoreProperty(1L) == 1, "1が変換できる")
    assert(property.fromStoreProperty(1.1) == 1, "1.1が変換できる")
    assert(property.fromStoreProperty(1.9) == 1, "1.9が変換できる")

    intercept[PropertyConvertFromLLPropertyException] {
      property.fromStoreProperty("A")
    }
    intercept[PropertyConvertFromLLPropertyException] {
      property.fromStoreProperty("1")
    }
  }

  test("toStorePropertyが正しいこと") {
    val property: Property[Long] = new ValueProperty[Long]("name")
    assert(property.toStoreProperty(1L) == 1L, "1Lが変換できる")
    assert(property.toStoreProperty(1) == 1L, "1Lが変換できる")
  }
}


class KeyPropertyTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  test("keyTypeが正しく取得できること") {

    val property = new KeyProperty[User]("name")

    assert(property.keyType == classOf[User])

  }

  test("toStorePropertyとfromStorePropertyが正しいこと") {

    val expected = Datastore.createKey[User]("hoge")
    val store = new KeyProperty("name").toStoreProperty(expected)

    assert(store == expected.key)

    val actual = new KeyProperty[User]("name").fromStoreProperty(store)
    assert(actual == expected)
  }

  test("toStorePropertyとfromStorePropertyが正しいこと2") {

    val expected = Datastore.createKey[User]("hoge")
    val p = new OptionProperty(new KeyProperty[User]("spouse")) with IndexedProperty[Option[Key[User]]]
    val store = p.toStoreProperty(Some(expected))

    assert(store == expected.key)

    val actual = new KeyProperty[User]("name").fromStoreProperty(store)
    assert(actual == expected)
  }

}

class EnumPropertyTest extends FunSuite {

  test("fromStorePropertyが正しいこと") {
    val actual = new EnumProperty[JavaEnum]("name").fromStoreProperty("JAVA_ENUM1")
    assert(actual == JavaEnum.JAVA_ENUM1)
  }

  test("toStorePropertyが正しいこと") {
    val actual = new EnumProperty[JavaEnum]("name").toStoreProperty(JavaEnum.JAVA_ENUM1)
    assert(actual == "JAVA_ENUM1")
  }

}

case class SampleCaseClass(name: String)

class SerializablePropertyTest extends FunSuite {

  test("Case ClassでfromStoreProperty->toStorePropertyが正しく動くこと") {
    val property: SerializableProperty[SampleCaseClass] = new SerializableProperty[SampleCaseClass]("name")
    val cc = SampleCaseClass("hoge hoge")
    val storedData = property.toStoreProperty(cc)
    val actual = property.fromStoreProperty(storedData)

    assert(actual == cc)
  }
}


class JsonPropertyTest extends FunSuite {

  test("Case ClassでfromStoreProperty->toStorePropertyが正しく動くこと") {
    val property: JsonProperty[SampleCaseClass] = new JsonProperty[SampleCaseClass]("name")
    val cc = SampleCaseClass("hoge hoge")
    val storedData = property.toStoreProperty(cc)

    assert(storedData == """{"name":"hoge hoge"}""")

    val actual = property.fromStoreProperty(storedData)

    assert(actual == cc)
  }
}
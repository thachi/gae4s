package com.xhachi.gae4s.datastore

import org.scalatest.FunSuite
import com.xhachi.gae4s.tests.AppEngineTestSuite
import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig

class LongPropertyTest extends FunSuite {


  test("fromStorePropertyが正しいこと") {
    val property: LongProperty = new LongProperty("name")

    assert(property.fromStoreProperty(1L) == 1L, "1Lが変換できる")
    assert(property.fromStoreProperty(1L) == 1, "1が変換できる")
    assert(property.fromStoreProperty(1.1) == 1, "1.1が変換できる")
    assert(property.fromStoreProperty(1.9) == 1, "1.9が変換できる")

    intercept[PropertyConversionException] {
      property.fromStoreProperty("A")
    }
    intercept[PropertyConversionException] {
      property.fromStoreProperty("1")
    }
  }

  test("toStorePropertyが正しいこと") {
    val property: LongProperty = new LongProperty("name")
    assert(property.toStoreProperty(1L) == 1L, "1Lが変換できる")
    assert(property.toStoreProperty(1) == 1L, "1Lが変換できる")
  }
}


class KeyPropertyTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  test("toStorePropertyとfromStorePropertyが正しいこと") {
    val m = new UserStore.Meta
    val expected = Key[User]("hoge")(m)
    val store = new KeyProperty("name").toStoreProperty(expected)

    assert(store == expected.key)

    val actual= new KeyProperty[User]("name").fromStoreProperty(store)
    assert(actual == expected)
  }

}

class EnumPropertyTest extends FunSuite {

  test("fromStorePropertyが正しいこと") {
    val actual = new EnumProperty[SampleEnum]("name").fromStoreProperty("A")
    assert(actual == SampleEnum.A)
  }

  test("toStorePropertyが正しいこと") {
    val actual = new EnumProperty[SampleEnum]("name").toStoreProperty(SampleEnum.A)
    assert(actual == "A")
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
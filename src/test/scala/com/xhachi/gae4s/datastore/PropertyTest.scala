package com.xhachi.gae4s.datastore

import org.scalatest.FunSuite

class LongPropertyTest extends FunSuite {


  test("fromStorePropertyが正しいこと") {
    val property: LongProperty = LongProperty("name")

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
    val property: LongProperty = LongProperty("name")
    assert(property.toStoreProperty(1L) == 1L, "1Lが変換できる")
    assert(property.toStoreProperty(1) == 1L, "1Lが変換できる")
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
    val property: SerializableProperty[SampleCaseClass] = SerializableProperty[SampleCaseClass]("name")
    val cc = SampleCaseClass("hoge hoge")
    val storedData = property.toStoreProperty(cc)
    val actual = property.fromStoreProperty(storedData)

    assert(actual == cc)
  }
}


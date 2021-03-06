package com.xhachi.gae4s.datastore

import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig
import com.xhachi.gae4s.datastore.meta.property
import com.xhachi.gae4s.tests.AppEngineTestSuite
import org.scalatest.FunSuite


class DataEntityTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  test("typeにEntityでデータを保存し取得できること") {

    val key = Datastore.createKey[TestDataEntity]("hoge")
    val before = new TestDataEntity(key)
    before.data = TestData("Takashi", 5)

    Datastore.create(before)
    assert(key.kind == "com.xhachi.gae4s.datastore.TestDataEntity")
    assert(key.idOption.isEmpty)
    assert(key.nameOption.isDefined)
    assert(key.nameOption.get == "hoge")

    val after = Datastore.get(key)
    assert(after.key == before.key)
    assert(after.data == before.data)

    after.addAge(1)
    Datastore.update(after)

    val after2 = Datastore.get(key)
    assert(after2.key == before.key)
    assert(after2.data.name == before.data.name)
    assert(after2.data.age == before.data.age + 1)
  }
}

class TestDataEntity(val key: Key[TestDataEntity])
  extends Entity[TestDataEntity]
  with MutableEntity {

  @property(json = true) var data: TestData = TestData("unknown", 0)

  def addAge(a: Int) = data = data.copy(age = data.age + a)
}

case class TestData(name: String, age: Int)

package com.xhachi.gae4s.datastore

import org.scalatest.FunSuite
import com.xhachi.gae4s.tests.AppEngineTestSuite
import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig


class DataEntityTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  test("データを保存し取得できること") {

    val store = new DataEntityStore[SampleData] with NamedStore with UpdatableStore with CreatableStore
    import store._

    val key: Key[DataEntity[SampleData]] = store.createKey("hoge")
    val before = new DataEntity[SampleData](key)
    before.data = SampleData("Takashi", 5)

    before.create()
    assert(key.kind == "com.xhachi.gae4s.datastore.SampleData")
    assert(key.idOption.isEmpty)
    assert(key.nameOption.isDefined)
    assert(key.nameOption.get == "hoge")

    val after = key.get
    assert(after.key == before.key)
    assert(after.data == before.data)

    after.data = after.data.copy(age = after.data.age + 1)
    after.update()

    val after2 = key.get
    assert(after2.key == before.key)
    assert(after2.data.name == before.data.name)
    assert(after2.data.age == before.data.age + 1)


  }
}

case class SampleData(name: String, age: Int)


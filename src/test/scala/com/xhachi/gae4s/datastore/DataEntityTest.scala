package com.xhachi.gae4s.datastore

import org.scalatest.FunSuite
import com.xhachi.gae4s.tests.AppEngineTestSuite
import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig


class DataEntityTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  test("データを保存し取得できること") {

    val store = new DataEntityStore[SampleData] with UpdatableStore
    val key: Key[DataEntity[SampleData]] = store.createKey("hoge")
    val before = new DataEntity[SampleData](key)
    before.data = Some(SampleData("Takashi", 5))

    store.create(before)
    assert(key.kind == "com.xhachi.gae4s.datastore.SampleData")
    assert(key.id.isEmpty)
    assert(key.name.isDefined)
    assert(key.name.get == "hoge")

    val after = store.get(key)
    assert(after.key == before.key)
    assert(after.data == before.data)
    assert(after.data.get.name == before.data.get.name)
  }
}

case class SampleData(name: String, age: Int)


package com.xhachi.gae4s.datastore

import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig
import com.xhachi.gae4s.tests.AppEngineTestSuite
import org.scalatest.FunSuite


class DataEntityTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  implicit val context = NoAncestorEntityStoreContext

  test("typeにEntityでデータを保存し取得できること") {

    val key: Key[SampleDataEntity] = SampleDataEntityStore.createKeyWithName("hoge")
    val before = new SampleDataEntity(key)
    before.data = SampleData("Takashi", 5)

    SampleDataEntityStore.create(before)
    assert(key.kind == "com.xhachi.gae4s.datastore.SampleDataEntity")
    assert(key.idOption.isEmpty)
    assert(key.nameOption.isDefined)
    assert(key.nameOption.get == "hoge")

    val after = SampleDataEntityStore.get(key)
    assert(after.key == before.key)
    assert(after.data == before.data)

    after.data = after.data.addAge(1)
    SampleDataEntityStore.update(after)

    val after2 = SampleDataEntityStore.get(key)
    assert(after2.key == before.key)
    assert(after2.data.name == before.data.name)
    assert(after2.data.age == before.data.age + 1)
  }


}

class SampleDataEntity(val key: Key[SampleDataEntity])
  extends Entity[SampleDataEntity] with Mutable {

  var data: SampleData = SampleData("unknown", 0)
}

object SampleDataEntityStore
  extends EntityStore[SampleDataEntity]
  with SingleStore
  with CreatableStore
  with UpdatableStore
  with NamedStore {

  val meta: EntityMeta[SampleDataEntity] = EntityMeta.createMeta
}

case class SampleData(name: String, age: Int) {
  def addAge(a: Int) = copy(age = age + a)
}


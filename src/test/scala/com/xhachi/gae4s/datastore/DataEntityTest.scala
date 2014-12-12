package com.xhachi.gae4s.datastore

import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig
import com.xhachi.gae4s.tests.AppEngineTestSuite
import org.scalatest.FunSuite


class DataEntityTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  implicit val context = NoAncestorEntityStoreContext

  test("typeにEntityでデータを保存し取得できること") {

    val key: Key[SampleDataEntity] = SampleDataEntity.createKeyWithName("hoge")
    val before = new SampleDataEntity(key)
    before.data = SampleData("Takashi", 5)

    before.create()
    assert(key.kind == "com.xhachi.gae4s.datastore.SampleDataEntity")
    assert(key.idOption.isEmpty)
    assert(key.nameOption.isDefined)
    assert(key.nameOption.get == "hoge")

    val after = key.get
    assert(after.key == before.key)
    assert(after.data == before.data)

    after.data = after.data.addAge(1)
    after.update()

    val after2 = key.get
    assert(after2.key == before.key)
    assert(after2.data.name == before.data.name)
    assert(after2.data.age == before.data.age + 1)
  }


}

class SampleDataEntity(val key: Key[SampleDataEntity])
  extends JsonDataEntity[SampleDataEntity, SampleData] with Mutable {
  override var data: SampleData = SampleData("unknown", 0)
}

object SampleDataEntity
  extends EntityStore[SampleDataEntity]
  with SingleStore
  with CreatableStore
  with UpdatableStore
  with NamedStore {

  type META = Meta
  val meta = new Meta

  class Meta extends JsonDataEntityMeta[SampleDataEntity, SampleData] with MutableMeta {
    def createEntity(key: Key[SampleDataEntity]): SampleDataEntity = new SampleDataEntity(key)

  }

}

case class SampleData(name: String, age: Int) {
  def addAge(a: Int) = copy(age = age + a)
}


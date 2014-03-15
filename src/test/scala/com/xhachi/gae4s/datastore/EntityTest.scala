package com.xhachi.gae4s.datastore

import org.scalatest.FunSuite
import com.google.appengine.tools.development.testing.LocalMemcacheServiceTestConfig
import com.xhachi.gae4s.tests.AppEngineTestSuite
import scala.collection.JavaConversions._
import com.google.appengine.api.datastore.Blob
import scala.reflect.ClassTag
import com.xhachi.gae4s.datastore.DataEntity
import com.google.appengine.api.datastore

class EntityTestTest extends FunSuite with AppEngineTestSuite {


  test("Entityを取得してkeyとkindが正しいこと") {
    val entity = DataEntity(Sample1("Takashi", 99))

    assert(entity.kind == "StoreSample@DataEntity")
    assert(entity.key != null)
    assert(entity.key.kind != "StoreSample@DataEntity")
    assert(entity.key.id.isDefined)
    assert(entity.key.name.isEmpty)
  }

  test("nameを指定したEntityを取得してkeyのnameが正しいこと") {
    val entity = DataEntity("entityKeyName", Sample1("Takashi", 99))
    assert(entity.key.id.isEmpty)
    assert(entity.key.name.isDefined)
    assert(entity.key.name.get == "entityKeyName")
  }

  test("idを指定したEntityを取得してkeyのidが正しいこと") {
    val entity = DataEntity(123454321L, Sample1("Takashi", 99))
    assert(entity.key.id.isDefined)
    assert(entity.key.id.get == 123454321L)
    assert(entity.key.name.isEmpty)
  }

/*
  test("EntityをLLEntityに変換できること") {
    val entity = DataEntity(Sample("Takashi", 99))
    val llentity = entity.toLLEntity
    assert(llentity.getKind == "StoreSample@DataEntity")

    val props = llentity.getProperties
    assert(props.size == 1)
    assert(props.containsKey("data"))

    val dataProperty = props.get("data").asInstanceOf[Blob]
    assert(dataProperty.getBytes.size != 0)
  }

  test("EntityをLLEntityに変換し戻すことができること") {
    val entity = DataEntity(Sample("Takashi", 99))
    val lle = entity.toLLEntity
    val e = EntityConverter.toDataEntity[Sample](lle)
    assert(e.data.name == "Takashi")

    //    val e: DataEntity[StoreSample] = new EntityConverter[DataEntity[StoreSample]].convert(llentity)

    //    val s = new EntityConverter().convert[DataEntity[StoreSample2]](llentity)

    //    DataEntity.getByKeyName[Sample]("name")
  }

*/
}

case class Sample1(name: String, age: Int)

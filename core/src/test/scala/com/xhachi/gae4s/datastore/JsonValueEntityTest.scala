package com.xhachi.gae4s.datastore

import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig
import com.xhachi.gae4s.datastore.annotations.json
import com.xhachi.gae4s.tests.AppEngineTestSuite
import org.scalatest.FunSuite

class JsonValueEntityTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  test("SimpleValueEntityのMetaが正しく生成されること") {

    val meta = EntityMeta.createMeta[JsonValueEntity]

    assert(meta.properties.size == 3)

    for (p <- meta.properties) {
      assert(!p.isInstanceOf[IndexedProperty[_]], p.name)
    }

    assert(meta.property("json").get.isInstanceOf[JsonProperty[_]])
    assert(meta.property("json").get.propertyType == classOf[JsonValue])
    assert(meta.property("jsonOption").get.isInstanceOf[OptionProperty[_]])
    assert(meta.property("jsonOption").get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[JsonProperty[_]])
    assert(meta.property("jsonOption").get.asInstanceOf[OptionProperty[_]].property.propertyType == classOf[JsonValue])
    assert(meta.property("jsonSeq").get.isInstanceOf[JsonProperty[_]])
    assert(meta.property("jsonSeq").get.propertyType == classOf[Seq[JsonValue]])
  }

  test("保存して読み込めること") {

    val key = Datastore.allocateKey[JsonValueEntity]
    val e = new JsonValueEntity(key)
    e.json = new JsonValue("hoge")
    e.jsonOption = Some(new JsonValue("hoge"))
    Datastore.put(e)

    val a = Datastore.get(key)
    assert(e.json == a.json)
    assert(e.jsonOption == a.jsonOption)
//    assert(e.jsonSeq == a.jsonSeq)

  }
}

class JsonValueEntity(val key: Key[JsonValueEntity]) extends Entity[JsonValueEntity] {
  @json var json: JsonValue = JsonValue("test")
  @json var jsonOption: Option[JsonValue] = None
  @json var jsonSeq: Seq[JsonValue] = Nil
}


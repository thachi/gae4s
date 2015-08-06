package com.xhachi.gae4s.datastore

import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig
import com.xhachi.gae4s.datastore.meta.property
import com.xhachi.gae4s.tests.AppEngineTestSuite
import org.scalatest.FunSuite


class SeqStoreTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  implicit val meta = EntityMeta.createMeta[SeqEntry]

  class SeqEntry(val key: Key[SeqEntry]) extends Entity[SeqEntry] {
    var name: String = null

    @property(indexed = true)
    var age: Int = 0
  }

  object SeqEntry {
    def apply(key: Key[SeqEntry], name: String, age: Int): SeqEntry = {
      val e = new SeqEntry(key)
      e.name = name
      e.age = age
      e
    }
  }

  lazy val emptyStore = SeqStore[SeqEntry](Seq())
  lazy val nonEmptyStore = SeqStore[SeqEntry](Seq(
    SeqEntry(meta.createKeyWithId(1), "taro", 10),
    SeqEntry(meta.createKeyWithId(2), "jiro", 8),
    SeqEntry(meta.createKeyWithId(3), "saburo", 7),
    SeqEntry(meta.createKeyWithId(4), "shiro", 5)
  ))

  test("countできること1") {
    val count = emptyStore.query.count
    assert(count == 0)
  }

  test("countできること2") {
    val count = nonEmptyStore.query.count
    assert(count == 4)
  }

  test("queryできること1") {
    val count = nonEmptyStore.count(Query[SeqEntry].filter(_.age == 7))
    assert(count == 1)
  }

  test("queryできること2") {
    val count = nonEmptyStore.count(Query[SeqEntry].filter(_.age < 7))
    assert(count == 1)
  }
  test("queryできること3") {
    val count = nonEmptyStore.count(Query[SeqEntry].filter(_.age <= 7))
    assert(count == 2)
  }

  test("queryできること4") {
    val count = nonEmptyStore.count(Query[SeqEntry].filter(_.age > 7))
    assert(count == 2)
  }
  test("queryできること5") {
    val count = nonEmptyStore.count(Query[SeqEntry].filter(_.age >= 7))
    assert(count == 3)
  }

}



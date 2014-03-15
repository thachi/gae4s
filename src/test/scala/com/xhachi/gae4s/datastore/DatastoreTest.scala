package com.xhachi.gae4s.datastore

import org.scalatest.FunSuite
import com.xhachi.gae4s.tests.AppEngineTestSuite
import com.google.appengine.tools.development.testing.{LocalDatastoreServiceTestConfig, LocalMemcacheServiceTestConfig}

case class Sample(key: Key[Sample], name: String) extends Entity[Sample]


class DatastoreTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  test("allocateしたKeyが取得できること") {
    val key = Datastore.allocateKey[Sample]()
    assert(key.name.isEmpty)
    assert(key.id.isDefined)
    assert(key.id.get > 0)
  }

  test("allocateしたKeyが取得できidが異なること") {
    val key1 = Datastore.allocateKey[Sample]()
    val key2 = Datastore.allocateKey[Sample]()
    assert(key1.id.get != key2.id.get)
  }

  test("IDを指定したKeyが取得できること") {
    val key = Datastore.createKey[Sample](1)
    assert(key.name.isEmpty)
    assert(key.id.isDefined)
    assert(key.id.get == 1)
  }

  test("Nameを指定したKeyが取得できること") {
    val key = Datastore.createKey[Sample]("key_name")
    assert(key.id.isEmpty)
    assert(key.name.isDefined)
    assert(key.name.get == "key_name")
  }


  test("getOptionできること") {
    val key = Datastore.createKey[Sample]("key_name")
    val created = Datastore.getOption(key)
    assert(created.isEmpty)
  }

  test("putできること") {
    val s = Sample(Datastore.createKey[Sample]("key_name"), "Hoge")
    Datastore.put(s)
  }

  test("countできること") {
    val count = Datastore.count(Datastore.query[Sample])
    assert(count == 0)
  }

  test("putしてcountが増えること") {
    val count1 = Datastore.count(Datastore.query[Sample])
    assert(count1 == 0)

    val s = Sample(Datastore.createKey[Sample]("key_name"), "Hoge")
    Datastore.put(s)

    val count2 = Datastore.count(Datastore.query[Sample])
    assert(count2 == 1)
  }

  test("putしてcountとasSeqの件数が等しいこと") {
    val s = Sample(Datastore.createKey[Sample]("key_name"), "Hoge")
    Datastore.put(s)

    val count = Datastore.count(Datastore.query[Sample])
    val seq = Datastore.asSeq(Datastore.query[Sample])
    assert(count == seq.size)
  }


}

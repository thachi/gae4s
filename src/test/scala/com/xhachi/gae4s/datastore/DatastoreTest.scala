package com.xhachi.gae4s.datastore

import org.scalatest.FunSuite
import com.xhachi.gae4s.tests.AppEngineTestSuite
import com.google.appengine.tools.development.testing.{LocalDatastoreServiceTestConfig, LocalMemcacheServiceTestConfig}



class DatastoreTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  test("allocateしたKeyが取得できること") {
    val key = Datastore.allocateKey[User]()
    assert(key.name.isEmpty)
    assert(key.id.isDefined)
    assert(key.id.get > 0)
  }

  test("allocateしたKeyが取得できidが異なること") {
    val key1 = Datastore.allocateKey[User]()
    val key2 = Datastore.allocateKey[User]()
    assert(key1.id.get != key2.id.get)
  }

  test("IDを指定したKeyが取得できること") {
    val key = Datastore.createKey[User](1)
    assert(key.name.isEmpty)
    assert(key.id.isDefined)
    assert(key.id.get == 1)
  }

  test("Nameを指定したKeyが取得できること") {
    val key = Datastore.createKey[User]("key_name")
    assert(key.id.isEmpty)
    assert(key.name.isDefined)
    assert(key.name.get == "key_name")
  }


  test("getOptionできること") {
    val key = Datastore.createKey[User]("key_name")
    val created = Datastore.getOption(key)
    assert(created.isEmpty)
  }

  test("putできること") {
    val s = User(Datastore.createKey[User]("key_name"), "Hoge")
    Datastore.put(s)
  }

  test("countできること") {
    val count = Datastore.count(Datastore.query[User])
    assert(count == 0)
  }

  test("putしてcountが増えること") {
    val count1 = Datastore.count(Datastore.query[User])
    assert(count1 == 0)

    val s = User(Datastore.createKey[User]("key_name"), "Hoge")
    Datastore.put(s)

    val count2 = Datastore.count(Datastore.query[User])
    assert(count2 == 1)
  }

  test("putしてcountとasSeqの件数が等しいこと") {
    val s = User(Datastore.createKey[User]("key_name"), "Hoge")
    Datastore.put(s)

    val count = Datastore.count(Datastore.query[User])
    val seq = Datastore.asSeq(Datastore.query[User])
    assert(count == seq.size)
  }

  test("putしてgetして等しいこと") {
    val key: Key[User] = Datastore.createKey[User]("key_name")
    val expected = User(key, "Hoge")
    Datastore.put(expected)

    val actual = Datastore.get(key)
    assert(actual == expected)
  }

}

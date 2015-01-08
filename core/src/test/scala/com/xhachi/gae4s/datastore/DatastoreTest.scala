package com.xhachi.gae4s.datastore

import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig
import com.xhachi.gae4s.tests.AppEngineTestSuite
import org.scalatest.FunSuite


class DatastoreTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  implicit val meta = EntityMeta.createMeta[User]


  test("allocateしたKeyが取得できること") {
    val key = Datastore.allocateKey[User]
    assert(key.nameOption.isEmpty)
    assert(key.idOption.isDefined)
    assert(key.idOption.get > 0)
  }

  test("allocateしたKeyが取得できidが異なること") {
    val key1 = Datastore.allocateKey[User]
    val key2 = Datastore.allocateKey[User]
    assert(key1.idOption.get != key2.idOption.get)
  }

  test("IDを指定したKeyが取得できること") {
    val key = Datastore.createKey[User](1)
    assert(key.nameOption.isEmpty)
    assert(key.idOption.isDefined)
    assert(key.idOption.get == 1)
  }

  test("Nameを指定したKeyが取得できること") {
    val key = Datastore.createKey[User]("key_name")
    assert(key.idOption.isEmpty)
    assert(key.nameOption.isDefined)
    assert(key.nameOption.get == "key_name")
  }


  test("getOptionできること") {
    val key = Datastore.createKey[User]("key_name")
    val created = Datastore.getOption(key)
    assert(created.isEmpty)
  }

  test("putできること") {
    val s = new User(Datastore.createKey("key_name"))
    s.name = "Hoge"
    Datastore.put(s)
  }

  test("countできること") {
    val count = Datastore.count(Datastore.query[User])
    assert(count == 0)
  }

  test("putしてcountが増えること") {
    val count1 = Datastore.count(Datastore.query[User])
    assert(count1 == 0)

    val s = new User(Datastore.createKey("key_name"))
    s.name = "Hoge"
    Datastore.put(s)

    val count2 = Datastore.count(Datastore.query[User])
    assert(count2 == 1)
  }

  test("putしてcountとasSeqの件数が等しいこと") {
    val s = new User(Datastore.createKey("key_name"))
    s.name = "Hoge"
    Datastore.put(s)

    val count = Datastore.count(Datastore.query[User])
    val seq = Datastore.asSeq(Datastore.query[User])
    assert(count == seq.size)
  }

  test("putしてgetして等しいこと") {
    val key: Key[User] = Datastore.createKey("key_name")
    val expected = new User(key)
    expected.name = "Hoge"
    Datastore.put(expected)

    val actual = Datastore.get(key)
    assert(actual.name == expected.name)
    assert(actual.height == expected.height)
    assert(actual.deleted == expected.deleted)
    assert(actual.createdAt != null)
  }

  test("2つputしてgetで一度に取得できること") {

    val key1: Key[User] = Datastore.createKey("key_name1")
    val expected1 = new User(key1)
    expected1.name = "Hoge1"
    Datastore.put(expected1)
    val key2: Key[User] = Datastore.createKey("key_name2")

    val expected2 = new User(key2)
    expected2.name = "Hoge2"
    Datastore.put(expected2)

    val actual = Datastore.get(key1 :: key2 :: Nil)
    assert(actual.size == 2)

    assert(actual(key1).name == expected1.name)
    assert(actual(key1).height == expected1.height)
    assert(actual(key1).deleted == expected1.deleted)
    assert(actual(key1).createdAt != null)

    assert(actual(key2).name == expected2.name)
    assert(actual(key2).height == expected2.height)
    assert(actual(key2).deleted == expected2.deleted)
    assert(actual(key2).createdAt != null)
  }

}



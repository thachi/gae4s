package com.xhachi.gae4s.datastore

import org.scalatest.FunSuite
import com.xhachi.gae4s.tests.AppEngineTestSuite
import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig


class DatastoreTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  implicit val meta = new UserStore.Meta

  implicit val context = NoAncestorEntityStoreContext

  test("allocateしたKeyが取得できること") {
    val key = UserStore.allocateKey
    assert(key.nameOption.isEmpty)
    assert(key.idOption.isDefined)
    assert(key.idOption.get > 0)
  }

  test("allocateしたKeyが取得できidが異なること") {
    val key1 = UserStore.allocateKey
    val key2 = UserStore.allocateKey
    assert(key1.idOption.get != key2.idOption.get)
  }

  test("IDを指定したKeyが取得できること") {
    val key = UserStore.createKeyWithId(1)
    assert(key.nameOption.isEmpty)
    assert(key.idOption.isDefined)
    assert(key.idOption.get == 1)
  }

  test("Nameを指定したKeyが取得できること") {
    val key = UserStore.createKeyWithName("key_name")
    assert(key.idOption.isEmpty)
    assert(key.nameOption.isDefined)
    assert(key.nameOption.get == "key_name")
  }


  test("getOptionできること") {
    val key = UserStore.createKeyWithName("key_name")
    val created = Datastore.getOption(key)
    assert(created.isEmpty)
  }

  test("putできること") {
    val s = new User(UserStore.createKeyWithName("key_name"), "Hoge")
    Datastore.put(s)
  }

  test("countできること") {
    val count = Datastore.count(Datastore.query[User, UserStore.Meta])
    assert(count == 0)
  }

  test("putしてcountが増えること") {
    val count1 = Datastore.count(Datastore.query[User, UserStore.Meta])
    assert(count1 == 0)

    val s = new User(UserStore.createKeyWithName("key_name"), "Hoge")
    Datastore.put(s)

    val count2 = Datastore.count(Datastore.query[User, UserStore.Meta])
    assert(count2 == 1)
  }

  test("putしてcountとasSeqの件数が等しいこと") {
    val s = new User(UserStore.createKeyWithName("key_name"), "Hoge")
    Datastore.put(s)

    val count = Datastore.count(Datastore.query[User, UserStore.Meta])
    val seq = Datastore.asSeq(Datastore.query[User, UserStore.Meta])
    assert(count == seq.size)
  }

  test("putしてgetして等しいこと") {
    val key: Key[User] = UserStore.createKeyWithName("key_name")
    val expected = new User(key, "Hoge")
    Datastore.put(expected)

    val actual = Datastore.get(key)
    assert(actual.name == expected.name)
    assert(actual.height == expected.height)
    assert(actual.deleted == expected.deleted)
    assert(actual.createdAt.isDefined)

  }

}

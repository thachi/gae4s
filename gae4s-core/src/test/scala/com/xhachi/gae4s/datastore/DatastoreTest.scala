package com.xhachi.gae4s.datastore

import java.util.Date

import com.xhachi.gae4s.tests.{AppEngineTestSuite, LocalServiceTestConfig}
import org.scalatest.FunSuite

import scala.compat.Platform.ConcurrentModificationException


class DatastoreTest
  extends FunSuite
    with AppEngineTestSuite
    with LocalServiceTestConfig.Datastore {

  test("allocateしたKeyが取得できること") {
    val key = Datastore.allocateKey("user")
    assert(key.nameOption.isEmpty)
    assert(key.idOption.isDefined)
    assert(key.idOption.get > 0)
  }

  test("allocateしたKeyが取得できidが異なること") {
    val key1 = Datastore.allocateKey("user")
    val key2 = Datastore.allocateKey("user")
    assert(key1.idOption.get != key2.idOption.get)
  }

  test("IDを指定したKeyが取得できること") {
    val key = Datastore.createKey("user", 1)
    assert(key.nameOption.isEmpty)
    assert(key.idOption.isDefined)
    assert(key.idOption.get == 1)
  }

  test("Nameを指定したKeyが取得できること") {
    val key = Datastore.createKey("user", "key_name")
    assert(key.idOption.isEmpty)
    assert(key.nameOption.isDefined)
    assert(key.nameOption.get == "key_name")
  }


  test("getOptionできること") {
    val key = Datastore.createKey("user", "key_name")
    val created = Datastore.getOption(key)
    assert(created.isEmpty)
  }

  test("putできること") {
    val s = Entity(Datastore.createKey("user", "key_name"), Seq(UnindexedProperty("name", "Hoge")))
    Datastore.put(s)
  }

  test("putできること2") {
    val s = Entity(Datastore.createKey("user", "key_name"), Seq(UnindexedProperty("name", Seq("Hoge"))))
    Datastore.put(s)
  }

  test("countできること") {
    val count = Datastore.count(Query("user"))
    assert(count == 0)
  }

  test("putしてcountが増えること") {
    val count1 = Datastore.count(Query("user"))
    assert(count1 == 0)

    val s = Entity(Datastore.createKey("user", 1))
    Datastore.put(s)

    val count2 = Datastore.count(Query("user"))
    assert(count2 == 1)
  }

  test("putしてcountとasSeqの件数が等しいこと") {
    val s = Entity(Datastore.createKey("user", "key_name"), Seq(UnindexedProperty("name", "Hoge")))
    Datastore.put(s)

    val count = Datastore.count(Query("user"))
    val seq = Datastore.asSeq(Query("user"))
    assert(count == seq.size)
  }

  test("putしてgetして等しいこと") {
    val key: Key = Datastore.createKey("user", "key_name")
    val expected = Entity(key, Seq(
      IndexedProperty("name", "Taro"),
      IndexedProperty("height", 123L),
      UnindexedProperty("deleted", true),
      IndexedProperty("createdAt", new Date)
    ))
    Datastore.put(expected)

    val actual = Datastore.get(key)
    assert(actual.properties.find(_.name == "name").get.isInstanceOf[IndexedProperty[String]])
    assert(actual.properties.find(_.name == "height").get.isInstanceOf[IndexedProperty[Long]])
    assert(actual.properties.find(_.name == "deleted").get.isInstanceOf[UnindexedProperty[Boolean]])
    assert(actual.properties.find(_.name == "createdAt").get.isInstanceOf[IndexedProperty[Date]])

    assert(actual[String]("name") == expected[String]("name"))
    assert(actual[Long]("height") == expected[Long]("height"))
    assert(actual[Boolean]("deleted") == expected[Boolean]("deleted"))
    assert(actual[Date]("createdAt") != null)
  }

  test("putしてgetProjectionして正しいこと") {
    val key: Key = Datastore.createKey("user", "key_name")
    val expected = Entity(key, Seq(
      IndexedProperty("name", "Taro"),
      IndexedProperty("height", 123L),
      UnindexedProperty("deleted", true),
      IndexedProperty("createdAt", new Date)
    ))
    Datastore.put(expected)

    val actual = Datastore.getProjection(key, Map("name" -> classOf[String]))
    assert(actual[String]("name") == expected[String]("name"))
  }

  test("2つputしてgetで一度に取得できること") {

    val key1: Key = Datastore.createKey("user", "key_name1")
    val key2: Key = Datastore.createKey("user", "key_name2")

    val expected1 = Entity(key1, Seq(
      UnindexedProperty("name", "Hoge1"),
      UnindexedProperty("height", 123L),
      UnindexedProperty("deleted", false),
      UnindexedProperty("spouse", key2),
      IndexedProperty("createdAt", new Date)
    ))
    Datastore.put(expected1)

    val expected2 = Entity(key2, Seq(
      UnindexedProperty("name", "Hoge2"),
      UnindexedProperty("height", 112L),
      UnindexedProperty("deleted", true),
      IndexedProperty("createdAt", new Date)
    ))
    Datastore.put(expected2)

    val actual = Datastore.get(key1 :: key2 :: Nil)
    assert(actual.size == 2)

    assert(actual(key1)[String]("name") == expected1[String]("name"))
    assert(actual(key1)[Long]("height") == expected1[Long]("height"))
    assert(actual(key1)[Boolean]("deleted") == expected1[Boolean]("deleted"))
    assert(actual(key1)[Key]("spouse") == key2)
    assert(actual(key1)[Date]("createdAt") != null)

    assert(actual(key2)[String]("name") == expected2[String]("name"))
    assert(actual(key2)[Long]("height") == expected2[Long]("height"))
    assert(actual(key2)[Boolean]("deleted") == expected2[Boolean]("deleted"))
    assert(actual(key2)[Date]("createdAt") != null)
  }

  test("Versionプロパティが正しく処理されること") {
    val key: Key = Datastore.createKey("user", "key_name")
    val expected = Entity(key, Seq(
      IndexedProperty("name", "Taro"),
      IndexedProperty("height", 123L),
      IndexedProperty("deleted", true),
      IndexedProperty("createdAt", new Date),
      VersionProperty("version", 0)
    ))
    Datastore.create(expected)

    val get1 = Datastore.get(key)
    assert(get1[String]("name") == expected[String]("name"))
    assert(get1[Long]("height") == expected[Long]("height"))
    assert(get1[Boolean]("deleted") == expected[Boolean]("deleted"))
    assert(get1[Date]("createdAt") != null)
    assert(get1[Long]("version") == 1)

    Datastore.update(get1.versioned("version"))

    val get2 = Datastore.get(key)
    assert(get2[String]("name") == expected[String]("name"))
    assert(get2[Long]("height") == expected[Long]("height"))
    assert(get2[Boolean]("deleted") == expected[Boolean]("deleted"))
    assert(get2[Date]("createdAt") != null)
    assert(get2[Long]("version") == 2)

  }

  test("Versionプロパティが正しくない場合にupdateできないこと") {
    val key: Key = Datastore.createKey("user", "key_name")
    val original = Entity(key, Seq(
      IndexedProperty("name", "Taro"),
      VersionProperty("version", 0)
    ))
    Datastore.create(original)

    val get1 = Datastore.get(key)
    assert(get1[String]("name") == original[String]("name"))
    assert(get1[Long]("version") == 1)

    Datastore.update(get1.versioned("version"))

    intercept[ConcurrentModificationException] {
      Datastore.update(get1.versioned("version"))
    }

  }
  test("Versionプロパティが正しくない場合にupdateできないこと2") {
    val key1: Key = Datastore.createKey("user", "key_name1")
    val key2: Key = Datastore.createKey("user", "key_name2")
    val original1 = Entity(key1, Seq(
      IndexedProperty("name", "Taro"),
      VersionProperty("version", 0)
    ))
    val original2 = Entity(key2, Seq(
      IndexedProperty("name", "Jiro"),
      VersionProperty("version", 0)
    ))
    Datastore.create(original1 :: original2 :: Nil)

    val gets = Datastore.get(key1 :: key2 :: Nil)

    val get1 = gets(key1)
    assert(get1[String]("name") == original1[String]("name"))
    assert(get1[Long]("version") == 1)

    val get2 = gets(key2)
    assert(get2[String]("name") == original2[String]("name"))
    assert(get2[Long]("version") == 1)

    Datastore.update(get1.versioned("version") :: get2.versioned("version") :: Nil)

    intercept[ConcurrentModificationException] {
      Datastore.update(get1.versioned("version") :: get2.versioned("version") :: Nil)
    }

  }

}



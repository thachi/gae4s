package com.xhachi.gae4s.memcache

import com.google.appengine.tools.development.testing.LocalMemcacheServiceTestConfig
import com.xhachi.gae4s.tests.AppEngineTestSuite
import org.scalatest.FunSuite


class MemcacheTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalMemcacheServiceTestConfig :: super.getConfig

  test("Memcacheから存在しないキーを取得してNoneであること") {
    val actual = Memcache.get[String]("key")

    assert(actual.isEmpty)
    assert(actual == None)
  }
  test("Memcacheにputしてgetすることができること（文字列）") {
    Memcache.put("key", "value")
    val actual = Memcache.get[String]("key")

    assert(actual.isDefined)
    assert(actual.get == "value")
  }

  test("Memcacheにputしてgetすることができること（数値）") {
    Memcache.put("key", 123)
    val actual = Memcache.get[Int]("key")

    assert(actual.isDefined)
    assert(actual.get == 123)
  }

  test("MemcacheにputしたものがgetOrElseで正常に取得できること") {
    Memcache.put("key", 123)
    val actual = Memcache.getOrElse[Int]("key", 999)

    assert(actual == 123)
  }

  test("MemcacheにputしてないものがgetOrElseで正常に取得できること") {
    val actual = Memcache.getOrElse[Int]("key", 999)

    assert(actual == 999)
  }


  test("Memcacheにputしてgetすることができること（case class）") {
    Memcache.put("key", StoreSample("Takashi", 99))
    val actual = Memcache.get[StoreSample]("key")

    assert(actual.isDefined)
    assert(actual.get.name == "Takashi")
    assert(actual.get.age == 99)
  }

  test("Memcacheにputして違う方でgetして例外が発生すること") {
    Memcache.put("key", "value")

    val actual = Memcache.get[Int]("key")
    intercept[RuntimeException] {
      actual.get
    }
  }

  test("Memcacheにputしてdeleteしてgetできないこと") {
    Memcache.put("key", "value")
    Memcache.delete("key")
    val actual = Memcache.get[String]("key")

    assert(actual.isEmpty)
  }

  test("Memcacheにputしてdeleteした際にcontainsが正しいこと") {
    Memcache.put("key", "value")

    val containsBefore: Boolean = Memcache.contains("key")
    assert(containsBefore)

    Memcache.delete("key")
    val containsAfter: Boolean = Memcache.contains("key")
    val actual = Memcache.get[String]("key")

    assert(!containsAfter)
    assert(actual.isEmpty)
  }

  test("Memcacheにinclementして値が正しいこと") {

    (1 to 1000).foreach {
      i => Memcache.inclement("inclement", 1, Some(0))
    }

    val actual = Memcache.get[Long]("inclement")
    assert(actual.isDefined)
    assert(actual.get == 1000)
  }

  test("MemcacheのIdentifiableValueが正しく動くこと") {

    val key = "Identifiable test key"

    Memcache.put(key, 1)
    val id1 = Memcache.getIdentifiable[Int](key)

    assert(id1.value == 1)

    val res1 = Memcache.putIfUntouched(key, id1, 2)
    assert(res1)

    val id2 = Memcache.getIdentifiable[Int](key)
    val id3 = Memcache.getIdentifiable[Int](key)

    assert(id2.value == 2)
    assert(id3.value == 2)

    val res2 = Memcache.putIfUntouched(key, id2, 3)
    assert(res2)

    // 書けない
    val res3 = Memcache.putIfUntouched(key, id3, 4)
    assert(!res3)

    val id4 = Memcache.getIdentifiable[Int](key)
    assert(id4.value == 3)

    val actual = Memcache.get[Int](key)
    assert(actual.get == 3)
  }


  test("MemcacheのgetIdentifiableが正しく動くこと") {

    val key1 = "Identifiable test key1"
    val key2 = "Identifiable test key2"

    Memcache.put(key1, 10)
    val id11 = Memcache.getIdentifiable[Int](key1)
    Memcache.put(key2, 20)
    val id21 = Memcache.getIdentifiable[Int](key2)

    assert(id11.value == 10)
    assert(id21.value == 20)

    val res11 = Memcache.putIfUntouched(key1, id11, 11)
    assert(res11)

    val ids = Memcache.getIdentifiables[String, Int](Seq(key1, key2))

    ids.keys

    assert(ids(key1).value == 11)
    assert(ids(key2).value == 20)

  }


}

case class StoreSample(name: String, age: Int)


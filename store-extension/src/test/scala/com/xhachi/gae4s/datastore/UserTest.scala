package com.xhachi.gae4s.datastore

import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig
import com.xhachi.gae4s.tests.AppEngineTestSuite
import org.scalatest.{FunSuite, Matchers}


class UserTest extends FunSuite with AppEngineTestSuite with Matchers {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  implicit val context = NoAncestorEntityStoreContext

  implicit val meta = UserStore.meta

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
    val count = Datastore.count(Datastore.query[User])
    assert(count == 0)
  }

  test("putしてcountが増えること") {
    val count1 = Datastore.count(Datastore.query[User])
    assert(count1 == 0)

    val s = new User(UserStore.createKeyWithName("key_name"), "Hoge")
    Datastore.put(s)

    val count2 = Datastore.count(Datastore.query[User])
    assert(count2 == 1)
  }

  test("putしてcountとasSeqの件数が等しいこと") {
    val s = new User(UserStore.createKeyWithName("key_name"), "Hoge")
    Datastore.put(s)

    val count = Datastore.count(Datastore.query[User])
    val seq = Datastore.asSeq(Datastore.query[User])
    assert(count == seq.size)
  }

  test("putしてgetして等しいこと") {
    val key: Key[User] = UserStore.createKeyWithName("key_name")
    val expected = new User(key, "Hoge")
    Datastore.put(expected)

    val actual = Datastore.get(key)
    assert(actual.key == expected.key)
    assert(actual.name == expected.name)
    assert(actual.mobilePhone == expected.mobilePhone)

    assert(actual.name == expected.name)
    assert(actual.height == expected.height)
    assert(actual.deleted == expected.deleted)
    assert(actual.createdAt != null)
  }

  test("2つputしてgetで一度に取得できること") {

    val key1: Key[User] = UserStore.createKeyWithName("key_name1")
    val expected1 = new User(key1, "Hoge1")
    Datastore.put(expected1)
    val key2: Key[User] = UserStore.createKeyWithName("key_name2")

    val expected2 = new User(key2, "Hoge2")
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

  def createTaroJiroSaburo() = {
    val tato = new User(UserStore.createKeyWithName("key_name_1"))
    tato.name = "Taro"
    tato.height = 190
    tato.weight = 90
    tato.mobilePhone = Some("090-xxxx-xxxx")

    val jiro = new User(UserStore.createKeyWithName("key_name_2"))
    jiro.name = "Jiro"
    jiro.height = 200
    jiro.weight = 90
    jiro.deleted = true

    val saburo = new User(UserStore.createKeyWithName("key_name_3"))
    saburo.name = "Saburo"
    saburo.height = 150
    saburo.weight = 120
    saburo.mobilePhone = Some("080-yyyy-yyyy")
    saburo.deleted = true

    UserStore.create(tato)
    UserStore.create(jiro)
    UserStore.create(saburo)
    assert(UserStore.query.count == 3)
  }

  test("putしてcountとasSeqとasKeySeqの件数がすべて1であること") {
    val s = new User(UserStore.createKeyWithName("key_name"), "Hoge")
    UserStore.create(s)

    val count = UserStore.query.count
    val seq = UserStore.query.asSeq
    val keySeq = UserStore.query.asKeySeq
    assert(count == 1)
    assert(seq.size == 1)
    assert(keySeq.size == 1)
  }

  test("createしてgetしてversionが1、updateして2であること") {
    val key: Key[User] = UserStore.createKeyWithName("key_name")
    val u1 = new User(key, "Hoge")
    assert(u1.version == 0L)

    UserStore.create(u1)
    val u2 = UserStore.get(key)
    assert(u2.version == 1L)

    UserStore.update(u2)
    val u3 = UserStore.get(key)
    assert(u3.version == 2L)
  }

  test("createしてgetして2回UPDATEしてVersionチェックエラーになること") {
    val key: Key[User] = UserStore.createKeyWithName("key_name")
    val u1 = new User(key, "Hoge")
    assert(u1.version == 0L)

    UserStore.create(u1)
    val u21 = UserStore.get(key)
    val u22 = UserStore.get(key)
    assert(u21.version == 1L)

    UserStore.update(u21)
    val u31 = UserStore.get(key)
    assert(u31.version == 2L)

    intercept[Exception] {
      UserStore.update(u22)
    }
  }

  test("createしてgetして2回SeqでUPDATEしてVersionチェックエラーになること") {
    val key: Key[User] = UserStore.createKeyWithName("key_name")
    val u1 = new User(key, "Hoge")
    assert(u1.version == 0L)

    UserStore.create(u1)
    val u21 = UserStore.get(key)
    val u22 = UserStore.get(key)
    assert(u21.version == 1L)

    UserStore.update(Seq(u21))
    val u31 = UserStore.get(key)
    assert(u31.version == 2L)

    intercept[Exception] {
      UserStore.update(Seq(u21))
    }

  }

  test("createしてgetしてcreatedAtと設定され、updateしてcreatedAtが変更されないこと") {
    val key: Key[User] = UserStore.createKeyWithName("key_name")
    val u1 = new User(key, "Hoge")
    u1.createdAt should be(null)

    UserStore.create(u1)
    val u2 = UserStore.get(key)
    assert(u2.createdAt != null)

    UserStore.update(u2)
    val u3 = UserStore.get(key)
    assert(u3.createdAt != null)
    assert(u3.createdAt == u2.createdAt)
  }



  test("createしてgetしてupdatedAtと設定され、updateしてupdatedAtが変更されること") {
    val key: Key[User] = UserStore.createKeyWithName("key_name")
    val u1 = new User(key, "Hoge")
    u1.updatedAt should be(null)

    UserStore.create(u1)
    val u2 = UserStore.get(key)
    assert(u2.updatedAt != null)

    Thread.sleep(1)
    UserStore.update(u2)
    val u3 = UserStore.get(key)
    assert(u3.updatedAt != null)
    assert(u3.updatedAt != u2.updatedAt)
  }


  test("queryを試す") {
    val key: Key[User] = UserStore.createKeyWithName("key_name")
    val expected = new User(key, "Hoge")
    UserStore.create(expected)

    assert(UserStore.query.count == 1)

    val seq = UserStore.query.asSeq
    assert(seq.size == 1)
    assert(seq.head.key == expected.key)

    val single = UserStore.query.asSingle
    assert(single.key == expected.key)
  }

  test("filterを試す") {
    createTaroJiroSaburo()
    val all = UserStore.query.asSeq

    assert(UserStore.query.asSeq(all) == all)

    val filter = UserStore.query.filter(_.name == "Taro")
    assert(filter.asSeq.size == 1)
    assert(filter.asSeq(all).size == 1)
  }

  test("filterでasSingleを試す") {
    createTaroJiroSaburo()
    val all = UserStore.query.asSeq
    val filter = UserStore.query.filter(m => m.name == "Jiro")
    assert(filter.asSingle.name == "Jiro")
    assert(filter.asSingle(all).name == "Jiro")
  }

  test("asSingleでヒットしない場合") {
    createTaroJiroSaburo()
    val all = UserStore.query.asSeq
    val filter = UserStore.query.filter(m => m.name == "hogehoge")

    filter.asSingle(all) should be(null)
    intercept[IllegalArgumentException] {
      filter.asSingle should be(null)
    }
  }

  test("asSingleOptionで見つかった場合") {
    createTaroJiroSaburo()
    val all = UserStore.query.asSeq

    val filter = UserStore.query.filter(m => m.name == "Jiro")

    {
      val single = filter.asSingleOption
      assert(single.isDefined)
      assert(single.get.name == "Jiro")
    }
    {
      val single = filter.asSingleOption(all)
      assert(single.isDefined)
      assert(single.get.name == "Jiro")
    }
  }

  test("asSingleOptionで見つからない場合") {
    createTaroJiroSaburo()
    val all = UserStore.query.asSeq

    val filter = UserStore.query.filter(m => m.name == "hogehoge")
    assert(filter.asSingleOption.isEmpty)
    assert(filter.asSingleOption(all).isEmpty)
  }

  test("filterでandを試す") {
    createTaroJiroSaburo()
    val all = UserStore.query.asSeq

    val filter1 = UserStore.query.filter(m => (m.name == "Jiro") && (m.deleted == false))
    assert(filter1.asSeq.size == 0)
    assert(filter1.asSeq(all).size == 0)

    val filter2 = UserStore.query.filter(m => (m.name == "Jiro") && (m.deleted == true))
    assert(filter2.asSeq.size == 1)
    assert(filter2.asSeq(all).size == 1)

    val filter3 = UserStore.query.filter(m => (m.name == "Jiro") && (m.deleted == true))
    assert(filter2.asSeq.head.key == filter3.asSingle.key)
    assert(filter2.asSeq.head.key == filter3.asSingle(all).key)

  }

  test("filterでorを試す") {
    createTaroJiroSaburo()
    val all = UserStore.query.asSeq

    val filter1 = UserStore.query.filter(m => (m.name == "Jiro") || (m.name == "Taro"))
    assert(filter1.asSeq.size == 2)
    assert(filter1.asSeq(all).size == 2)

    val filter2 = UserStore.query.filter(m => (m.name == "Jiro") || (m.name == "Goro"))
    assert(filter2.asSeq.size == 1)
    assert(filter2.asSeq(all).size == 1)
  }

  ignore("filterでinを試す") {
    createTaroJiroSaburo()
    val all = UserStore.query.asSeq

    //    val filter1 = UserStore.query.filter(_.name in("Taro", "Jiro", "Saburo"))
    //    assert(filter1.asSeq.size == 3)
    //    assert(filter1.asSeq(all).size == 3)
    //
    //    val filter2 = UserStore.query.filter(_.name in("Jiro", "Taro"))
    //    assert(filter2.asSeq.size == 2)
    //    assert(filter2.asSeq(all).size == 2)
    //
    //    val filter3 = UserStore.query.filter(_.name in("Jiro", "Goro"))
    //    assert(filter3.asSeq.size == 1)
    //    assert(filter3.asSeq(all).size == 1)
  }

  test("filterで大小比較を試す") {
    createTaroJiroSaburo()
    val all = UserStore.query.asSeq

    val filter1 = UserStore.query.filter(_.height < 190)
    assert(filter1.asSeq.size == 1)
    assert(filter1.asSeq(all).size == 1)

    val filter2 = UserStore.query.filter(_.height <= 190)
    assert(filter2.asSeq.size == 2)
    assert(filter2.asSeq(all).size == 2)

    val filter3 = UserStore.query.filter(_.height > 190)
    assert(filter3.asSeq.size == 1)
    assert(filter3.asSeq(all).size == 1)

    val filter4 = UserStore.query.filter(_.height >= 190)
    assert(filter4.asSeq.size == 2)
    assert(filter4.asSeq(all).size == 2)
  }

  test("filterでnullでの検索を試す") {
    createTaroJiroSaburo()
    val all = UserStore.query.asSeq

    val filter1 = UserStore.query.filter(_.mobilePhone == None)
    assert(filter1.asSeq.size == 1)
    //FIXME: 今はまだOptionはフィルタできないです
    //    assert(filter1.asSeq(all).size == 1)

    val filter2 = UserStore.query.filter(_.mobilePhone != None)
    assert(filter2.asSeq.size == 2)
    //FIXME: 今はまだOptionはフィルタできないです
    //    assert(filter2.asSeq(all).size == 2)

    val filter3 = UserStore.query.filter(_.mobilePhone == null)
    assert(filter3.asSeq.size == 1)
    //FIXME: 今はまだOptionはフィルタできないです
    //    assert(filter3.asSeq(all).size == 1)

    val filter4 = UserStore.query.filter(_.mobilePhone != null)
    assert(filter4.asSeq.size == 2)
    //FIXME: 今はまだOptionはフィルタできないです
    //    assert(filter4.asSeq(all).size == 2)
  }
  //
  //  test("sortを試す") {
  //    createTaroJiroSaburo()
  //    val all = UserStore.query.asSeq
  //
  //
  //    val sort1 = UserStore.query.sort(_.height)
  //    val seq11 = sort1.asSeq
  //    assert(seq11.size == 3)
  //    assert(seq11(0).name == "Saburo")
  //    assert(seq11(1).name == "Taro")
  //    assert(seq11(2).name == "Jiro")
  //    val seq12 = sort1.asSeq(all)
  //    assert(seq12.size == 3)
  //    assert(seq12(0).name == "Saburo")
  //    assert(seq12(1).name == "Taro")
  //    assert(seq12(2).name == "Jiro")
  //
  //    val seq2 = UserStore.query.sort(_.height.desc, _.weight.desc).asSeq
  //    assert(seq2.size == 3)
  //    assert(seq2(0).name == "Jiro")
  //    assert(seq2(1).name == "Taro")
  //    assert(seq2(2).name == "Saburo")
  //
  //    val seq3 = UserStore.query.sort(_.weight.asc, _.height.desc).asSeq
  //    assert(seq3.size == 3)
  //    assert(seq3(0).name == "Jiro")
  //    assert(seq3(1).name == "Taro")
  //    assert(seq3(2).name == "Saburo")
  //
  //  }
  //
  //  test("offsetを試す") {
  //    createTaroJiroSaburo()
  //
  //    //基本は三郎、太郎、次郎の順。
  //    val all = UserStore.query.sort(_.height.asc).asSeq
  //    assert(all.size == 3)
  //    assert(all(0).name == "Saburo")
  //    assert(all(1).name == "Taro")
  //    assert(all(2).name == "Jiro")
  //
  //
  //    val sort1 = UserStore.query.sort(_.height.asc).offset(1)
  //    val seq11 = sort1.asSeq
  //    assert(seq11.size == 2)
  //    assert(seq11(0).name == "Taro")
  //    assert(seq11(1).name == "Jiro")
  //  }
  //
  //  test("limitを試す") {
  //    createTaroJiroSaburo()
  //    val all = UserStore.query.asSeq
  //
  //    val sort1 = UserStore.query.sort(_.height.asc).limit(2)
  //    val seq11 = sort1.asSeq
  //    assert(seq11.size == 2)
  //    assert(seq11(0).name == "Saburo")
  //    assert(seq11(1).name == "Taro")
  //  }
  //
  //  test("offsetとlimitを試す") {
  //    createTaroJiroSaburo()
  //    val all = UserStore.query.asSeq
  //
  //    val sort1 = UserStore.query.sort(_.height.asc).offset(1).limit(1)
  //    val seq11 = sort1.asSeq
  //    assert(seq11.size == 1)
  //    assert(seq11(0).name == "Taro")
  //  }

}

class UserStore
  extends EntityStore[User]
  with NamedStore
  with IdentifiableKeyStore
  with AllocatableKeyStore
  with CreatableStore
  with UpdatableStore
  with QueryableStore {

  val meta = EntityMeta.createMeta[User]
}

object UserStore extends UserStore

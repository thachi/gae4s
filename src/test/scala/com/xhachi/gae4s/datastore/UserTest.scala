package com.xhachi.gae4s.datastore

import com.google.appengine.api.datastore.{Entity => LLEntity}
import org.scalatest.FunSuite
import com.xhachi.gae4s.tests.AppEngineTestSuite
import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig


class UserTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  def createTaroJiroSaburo = {
    val tato = new User(UserStore.createKey("key_name_1"), "Taro", height = 190, weight = 90, mobilePhone = Some("090-xxxx-xxxx"))
    val jiro = new User(UserStore.createKey("key_name_2"), "Jiro", height = 200, weight = 90, deleted = true)
    val saburo = new User(UserStore.createKey("key_name_3"), "Saburo", height = 150, weight = 120, mobilePhone = Some("080-yyyy-yyyy"), deleted = true)
    UserStore.create(tato)
    UserStore.create(jiro)
    UserStore.create(saburo)
    assert(UserStore.query.count == 3)
  }

  test("putしてcountとasSeqとasKeySeqの件数がすべて1であること") {
    val s = new User(UserStore.createKey("key_name"), "Hoge")
    UserStore.create(s)

    val count = UserStore.query.count
    val seq = UserStore.query.asSeq
    val keySeq = UserStore.query.asKeySeq
    assert(count == 1)
    assert(seq.size == 1)
    assert(keySeq.size == 1)
  }

  test("putしてgetして等しいこと") {
    val key: Key[User] = UserStore.createKey("key_name")
    val expected = new User(key, "Hoge")
    UserStore.create(expected)

    val actual = UserStore.get(key)
    assert(actual.key == expected.key)
    assert(actual.name == expected.name)
  }

  test("createしてgetしてversionが1、updateして2であること") {
    val key: Key[User] = UserStore.createKey("key_name")
    val u1 = new User(key, "Hoge")
    assert(u1.version == 0L)

    UserStore.create(u1)
    val u2 = UserStore.get(key)
    assert(u2.version == 1L)

    UserStore.update(u2)
    val u3 = UserStore.get(key)
    assert(u3.version == 2L)
  }

  test("createしてgetしてcreatedAtと設定され、updateしてcreatedAtが変更されないこと") {
    val key: Key[User] = UserStore.createKey("key_name")
    val u1 = new User(key, "Hoge")
    assert(u1.createdAt.isEmpty)

    UserStore.create(u1)
    val u2 = UserStore.get(key)
    assert(u2.createdAt.isDefined)

    UserStore.update(u2)
    val u3 = UserStore.get(key)
    assert(u3.createdAt.isDefined)
    assert(u3.createdAt.get == u2.createdAt.get)
  }

  test("createしてgetしてupdatedAtと設定され、updateしてupdatedAtが変更されること") {
    val key: Key[User] = UserStore.createKey("key_name")
    val u1 = new User(key, "Hoge")
    assert(u1.updatedAt.isEmpty)

    UserStore.create(u1)
    val u2 = UserStore.get(key)
    assert(u2.updatedAt.isDefined)

    Thread.sleep(1)
    UserStore.update(u2)
    val u3 = UserStore.get(key)
    assert(u3.updatedAt.isDefined)
    assert(u3.updatedAt.get != u2.updatedAt.get)
  }


  test("queryを試す") {
    val key: Key[User] = UserStore.createKey("key_name")
    val expected = new User(key, "Hoge")
    UserStore.create(expected)

    assert(UserStore.query.count == 1)

    val seq = UserStore.query.asSeq
    assert(seq.size == 1)
    assert(seq.head.key == expected.key)
  }

  test("propertyが正しいか") {
    val filter = new UserMeta().name #< "a"
    filter.toLLFilter
  }

  test("filterを試す") {
    createTaroJiroSaburo

    val seq = UserStore.query.filter(_.name #== "Taro").asSeq
    assert(seq.size == 1)
  }

  test("filterでandを試す") {
    createTaroJiroSaburo

    val seq1 = UserStore.query.filter(m => (m.name #== "Jiro") && (m.deleted #== false)).asSeq
    assert(seq1.size == 0)

    val seq2 = UserStore.query.filter(m => (m.name #== "Jiro") && (m.deleted #== true)).asSeq
    assert(seq2.size == 1)
  }

  test("filterでorを試す") {
    createTaroJiroSaburo

    val seq1 = UserStore.query.filter(m => (m.name #== "Jiro") || (m.name #== "Taro")).asSeq
    assert(seq1.size == 2)

    val seq2 = UserStore.query.filter(m => (m.name #== "Jiro") || (m.name #== "Goro")).asSeq
    assert(seq2.size == 1)
  }

  test("filterでinを試す") {
    createTaroJiroSaburo

    val seq1 = UserStore.query.filter(_.name in("Taro", "Jiro", "Saburo")).asSeq
    assert(seq1.size == 3)

    val seq2 = UserStore.query.filter(_.name in("Jiro", "Taro")).asSeq
    assert(seq2.size == 2)

    val seq3 = UserStore.query.filter(_.name in("Jiro", "Goro")).asSeq
    assert(seq3.size == 1)
  }

  test("filterで大小比較を試す") {
    createTaroJiroSaburo

    val seq1 = UserStore.query.filter(_.height #< 190).asSeq
    assert(seq1.size == 1)

    val seq2 = UserStore.query.filter(_.height #<= 190).asSeq
    assert(seq2.size == 2)

    val seq3 = UserStore.query.filter(_.height #> 190).asSeq
    assert(seq3.size == 1)

    val seq4 = UserStore.query.filter(_.height #>= 190).asSeq
    assert(seq4.size == 2)
  }

  test("sortを試す") {
    createTaroJiroSaburo


    val seq1 = UserStore.query.sort(_.height.asc).asSeq
    assert(seq1.size == 3)
    assert(seq1(0).name == "Saburo")
    assert(seq1(1).name == "Taro")
    assert(seq1(2).name == "Jiro")

    val seq2 = UserStore.query.sort(_.height.desc, _.weight.desc).asSeq
    assert(seq2.size == 3)
    assert(seq2(0).name == "Jiro")
    assert(seq2(1).name == "Taro")
    assert(seq2(2).name == "Saburo")

    val seq3 = UserStore.query.sort(_.weight.asc, _.height.desc).asSeq
    assert(seq3.size == 3)
    assert(seq3(0).name == "Jiro")
    assert(seq3(1).name == "Taro")
    assert(seq3(2).name == "Saburo")

  }

}

class User(
            val key: Key[User],
            var name: String = "",
            var height: Int = 0,
            var weight: Int = 0,
            var mobilePhone: Option[String] = None,
            var webInfo: WebInfo = WebInfo(),
            var deleted: Boolean = false
            ) extends Entity[User] with CreatedAt with Version with UpdatedAt

case class WebInfo(email: Option[String] = None, twitter: Option[String] = None)



class UserStore
  extends EntityStore[User]
  with QueryableStore
  with UpdatableStore
  with NamedStore
  with IdentifiableKeyStore
  with AllocatableKeyStore {

  override type ENTITY = User
  override type META = UserMeta

  implicit val meta = new UserMeta
}

object UserStore extends UserStore


class UserMeta extends EntityMeta[User] with CreatedAtMeta with UpdatedAtMeta with VersionMeta {

  val key = new KeyProperty("key")
  val name = new StringProperty("name") with IndexedProperty[String]
  val height = new IntProperty("height") with IndexedProperty[Int]
  val weight = new IntProperty("weight") with IndexedProperty[Int]
  val mobilePhone = new OptionProperty(new StringProperty("mobilePhone"))
  val webInfo = new SerializableProperty[WebInfo]("webInfo")
  val deleted = new BooleanProperty("deleted") with IndexedProperty[Boolean]

  override def createEntity(key: Key[User]) = new User(key)

  addApplyFromLLEntity {
    (from: LLEntity, to: User) =>
      to.name = name.getFromStore(from)
      to.height = height.getFromStore(from)
      to.weight = weight.getFromStore(from)
      to.mobilePhone = mobilePhone.getFromStore(from)
      to.webInfo = webInfo.getFromStore(from)
      to.deleted = deleted.getFromStore(from)
  }

  addApplyToLLEntity {
    (entity: User, e: LLEntity) =>
      name.setToStore(entity.name)(e)
      height.setToStore(entity.height)(e)
      weight.setToStore(entity.weight)(e)
      webInfo.setToStore(entity.webInfo)(e)
      deleted.setToStore(entity.deleted)(e)
  }
}

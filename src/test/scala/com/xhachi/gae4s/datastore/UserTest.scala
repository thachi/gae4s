package com.xhachi.gae4s.datastore

import java.util.Date
import com.google.appengine.api.datastore.{Entity => LLEntity}
import org.scalatest.FunSuite
import com.xhachi.gae4s.tests.AppEngineTestSuite
import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig
import com.xhachi.gae4s.datastore.User.UserMeta


class UserTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  def createTaroJiroSaburo = {
    val tato = User(User.createKey("key_name_1"), "Taro", height = 190, weight = 90, mobilePhone = Some("090-xxxx-xxxx"))
    val jiro = User(User.createKey("key_name_2"), "Jiro", height = 200, weight = 90, deleted = true)
    val saburo = User(User.createKey("key_name_3"), "Saburo", height = 150, weight = 120, mobilePhone = Some("080-yyyy-yyyy"), deleted = true)
    User.create(tato)
    User.create(jiro)
    User.create(saburo)
    assert(User.query.count == 3)
  }

  test("putしてcountとasSeqとasKeySeqの件数がすべて1であること") {
    val s = User(User.createKey("key_name"), "Hoge")
    User.create(s)

    val count = User.query.count
    val seq = User.query.asSeq
    val keySeq = User.query.asKeySeq
    assert(count == 1)
    assert(seq.size == 1)
    assert(keySeq.size == 1)
  }

  test("putしてgetして等しいこと") {
    val key: Key[User] = User.createKey("key_name")
    val expected = User(key, "Hoge")
    User.create(expected)

    val actual = User.get(key)
    assert(actual == expected)
  }

  test("queryを試す") {
    val key: Key[User] = User.createKey("key_name")
    val expected = User(key, "Hoge")
    User.create(expected)

    assert(User.query.count == 1)

    val seq = User.query.asSeq
    assert(seq.size == 1)
    assert(seq.head == expected)
  }

  test("propertyが正しいか") {
    val filter = new UserMeta().name #< "a"
    filter.toLLFilter
  }

  test("filterを試す") {
    createTaroJiroSaburo

    val seq = User.query.filter(_.name #== "Taro").asSeq
    assert(seq.size == 1)
  }

  test("filterでandを試す") {
    createTaroJiroSaburo

    val seq1 = User.query.filter(m => (m.name #== "Jiro") && (m.deleted #== false)).asSeq
    assert(seq1.size == 0)

    val seq2 = User.query.filter(m => (m.name #== "Jiro") && (m.deleted #== true)).asSeq
    assert(seq2.size == 1)
  }

  test("filterでorを試す") {
    createTaroJiroSaburo

    val seq1 = User.query.filter(m => (m.name #== "Jiro") || (m.name #== "Taro")).asSeq
    assert(seq1.size == 2)

    val seq2 = User.query.filter(m => (m.name #== "Jiro") || (m.name #== "Goro")).asSeq
    assert(seq2.size == 1)
  }

  test("filterでinを試す") {
    createTaroJiroSaburo

    val seq1 = User.query.filter(_.name in("Taro", "Jiro", "Saburo")).asSeq
    assert(seq1.size == 3)

    val seq2 = User.query.filter(_.name in("Jiro", "Taro")).asSeq
    assert(seq2.size == 2)

    val seq3 = User.query.filter(_.name in("Jiro", "Goro")).asSeq
    assert(seq3.size == 1)
  }

  test("filterで大小比較を試す") {
    createTaroJiroSaburo

    val seq1 = User.query.filter(_.height #< 190).asSeq
    assert(seq1.size == 1)

    val seq2 = User.query.filter(_.height #<= 190).asSeq
    assert(seq2.size == 2)

    val seq3 = User.query.filter(_.height #> 190).asSeq
    assert(seq3.size == 1)

    val seq4 = User.query.filter(_.height #>= 190).asSeq
    assert(seq4.size == 2)
  }

  test("sortを試す") {
    createTaroJiroSaburo


    val seq1 = User.query.sort(_.height.asc).asSeq
    assert(seq1.size == 3)
    assert(seq1(0).name == "Saburo")
    assert(seq1(1).name == "Taro")
    assert(seq1(2).name == "Jiro")

    val seq2 = User.query.sort(_.height.desc, _.weight.desc).asSeq
    assert(seq2.size == 3)
    assert(seq2(0).name == "Jiro")
    assert(seq2(1).name == "Taro")
    assert(seq2(2).name == "Saburo")

    val seq3 = User.query.sort(_.weight.asc, _.height.desc).asSeq
    assert(seq3.size == 3)
    assert(seq3(0).name == "Jiro")
    assert(seq3(1).name == "Taro")
    assert(seq3(2).name == "Saburo")

  }

}

case class User(
                 key: Key[User],
                 name: String,
                 height: Int = 0,
                 weight: Int = 0,
                 mobilePhone: Option[String] = None,
                 webInfo: WebInfo = WebInfo(),
                 createdAt: Date = new Date,
                 deleted: Boolean = false
                 ) extends Entity[User]

case class WebInfo(email: Option[String] = None, twitter: Option[String] = None)


object User extends Storable with Queryable with Mutable with NamedKey with IdentifiableKey with AutoAllocateKey {
  type E = User
  type M = UserMeta
  val datastore = Datastore
  implicit val meta = new UserMeta


  class UserMeta extends EntityMeta[User] {

    val kind = "com.example.User"

    val key = new KeyProperty("key", this)
    val name = new StringProperty("name") with IndexedProperty[String]
    val height = new IntProperty("height") with IndexedProperty[Int]
    val weight = new IntProperty("weight") with IndexedProperty[Int]
    val mobilePhone = new OptionProperty(new StringProperty("mobilePhone"))
    val webInfo = new SerializableProperty[WebInfo]("webInfo")
    val createAt = new DateProperty("createAt")
    val deleted = new BooleanProperty("deleted") with IndexedProperty[Boolean]


    override def fromLLEntity(entity: LLEntity): User = {
      User(
        Key(entity.getKey),
        name.getFromStore(entity),
        height.getFromStore(entity),
        weight.getFromStore(entity),
        mobilePhone.getFromStore(entity),
        webInfo.getFromStore(entity),
        createAt.getFromStore(entity),
        deleted.getFromStore(entity)
      )
    }

    override def toLLEntity(entity: User): LLEntity = {
      implicit val e = createLLEntity(entity)
      name.setToStore(entity.name)
      height.setToStore(entity.height)
      weight.setToStore(entity.weight)
      createAt.setToStore(entity.createdAt)
      webInfo.setToStore(entity.webInfo)
      deleted.setToStore(entity.deleted)
      e
    }

  }

}

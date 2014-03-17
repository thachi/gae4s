package com.xhachi.gae4s.datastore

import java.util.Date
import com.google.appengine.api.datastore
import org.scalatest.FunSuite
import com.xhachi.gae4s.tests.AppEngineTestSuite
import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig

class UserTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  test("putしてcountとasSeqの件数が等しいこと") {
    val s = User(Datastore.createKey[User]("key_name"), "Hoge")
    User.create(s)

    val count = Datastore.count(User.query)
    val seq = Datastore.asSeq(User.query)
    assert(count == seq.size)
    assert(count == 1)
  }

  test("putしてgetして等しいこと") {
    val key: Key[User] = Datastore.createKey[User]("key_name")
    val expected = User(key, "Hoge")
    User.create(expected)

    val actual = User.get(key)
    assert(actual == expected)
  }

  test("queryを試す") {
    val key: Key[User] = Datastore.createKey[User]("key_name")
    val expected = User(key, "Hoge")
    User.create(expected)

    assert(User.query.count == 1)

    val seq = User.query.asSeq
    assert(seq.size == 1)
    assert(seq.head == expected)
  }

  test("filterを試す") {
    val tato = User(Datastore.createKey[User]("key_name_1"), "Taro")
    val jiro = User(Datastore.createKey[User]("key_name_2"), "Jiro")
    User.create(tato)
    User.create(jiro)

    assert(User.query.count == 2)

    val seq = User.query.filter(_.name #== "Taro").asSeq
    assert(seq.size == 1)
  }

  test("filterでandを試す") {
    val tato = User(Datastore.createKey[User]("key_name_1"), "Taro")
    val jiro = User(Datastore.createKey[User]("key_name_2"), "Jiro", deleted = true)
    val saburo = User(Datastore.createKey[User]("key_name_3"), "Saburo", deleted = true)
    User.create(tato)
    User.create(jiro)
    User.create(saburo)

    assert(User.query.count == 3)

    val seq1 = User.query.filter(m => (m.name #== "Jiro") && (m.deleted #== false)).asSeq
    assert(seq1.size == 0)

    val seq2 = User.query.filter(m => (m.name #== "Jiro") && (m.deleted #== true)).asSeq
    assert(seq2.size == 1)
  }

  test("filterでorを試す") {
    val tato = User(Datastore.createKey[User]("key_name_1"), "Taro")
    val jiro = User(Datastore.createKey[User]("key_name_2"), "Jiro", deleted = true)
    val saburo = User(Datastore.createKey[User]("key_name_3"), "Saburo", deleted = true)
    User.create(tato)
    User.create(jiro)
    User.create(saburo)
    assert(User.query.count == 3)

    val seq1 = User.query.filter(m => (m.name #== "Jiro") || (m.name #== "Taro")).asSeq
    assert(seq1.size == 2)

    val seq2 = User.query.filter(m => (m.name #== "Jiro") || (m.name #== "Goro")).asSeq
    assert(seq2.size == 1)
  }

  test("filterでinを試す") {
    val tato = User(Datastore.createKey[User]("key_name_1"), "Taro")
    val jiro = User(Datastore.createKey[User]("key_name_2"), "Jiro", deleted = true)
    val saburo = User(Datastore.createKey[User]("key_name_3"), "Saburo", deleted = true)
    User.create(tato)
    User.create(jiro)
    User.create(saburo)
    assert(User.query.count == 3)

    val seq1 = User.query.filter(_.name in("Taro", "Jiro", "Saburo")).asSeq
    assert(seq1.size == 3)

    val seq2 = User.query.filter(_.name in("Jiro", "Taro")).asSeq
    assert(seq2.size == 2)

    val seq3 = User.query.filter(_.name in("Jiro", "Goro")).asSeq
    assert(seq3.size == 1)
  }

  test("filterで大小比較を試す") {
    val tato = User(Datastore.createKey[User]("key_name_1"), "Taro", height = 190, weight = 90)
    val jiro = User(Datastore.createKey[User]("key_name_2"), "Jiro", height = 200, weight = 100, deleted = true)
    val saburo = User(Datastore.createKey[User]("key_name_3"), "Saburo", height = 150, weight = 120, deleted = true)
    User.create(tato)
    User.create(jiro)
    User.create(saburo)
    assert(User.query.count == 3)

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
    val tato = User(Datastore.createKey[User]("key_name_1"), "Taro", height = 190, weight = 90)
    val jiro = User(Datastore.createKey[User]("key_name_2"), "Jiro", height = 200, weight = 90, deleted = true)
    val saburo = User(Datastore.createKey[User]("key_name_3"), "Saburo", height = 150, weight = 120, deleted = true)
    User.create(tato)
    User.create(jiro)
    User.create(saburo)
    assert(User.query.count == 3)

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
                 webInfo: WebInfo = WebInfo(),
                 createdAt: Date = new Date,
                 deleted: Boolean = false
                 ) extends Entity[User]

case class WebInfo(email: Option[String] = None, twitter: Option[String] = None)

object UserMeta extends UserMeta

class UserMeta private() extends EntityMeta[User] {

  val name = StringProperty("name")
  val height = IntProperty("height")
  val weight = IntProperty("weight")
  val webInfo = SerializableProperty[WebInfo]("webInfo")
  val createAt = DateProperty("createAt")
  val deleted = BooleanProperty("deleted")

  override def properties: Seq[Property[_, _]] = createAt :: Nil

  override def fromLLEntity(entity: datastore.Entity): User = {
    User(
      entity.getKey,
      name.getFromStore(entity),
      height.getFromStore(entity),
      weight.getFromStore(entity),
      webInfo.getFromStore(entity),
      createAt.getFromStore(entity),
      deleted.getFromStore(entity)
    )
  }

  override def toLLEntity(entity: User): datastore.Entity = {
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

object User extends Storable with Queryable with Mutable {
  type E = User
  type M = UserMeta
  val datastore = Datastore
  implicit val meta = UserMeta
}

trait Storable {
  protected type E <: Entity[E]
  protected type M <: EntityMeta[E]
  protected implicit val meta: M

  protected def datastore: Datastore

  def get(key: Key[E]) = datastore.get(key)

  def create(e: E) = datastore.create(e)
}

trait Queryable extends Storable {
  def query: Query[E, M] = datastore.query[E, M]
}

trait Mutable extends Storable {
  def update(e: E) = datastore.update(e)
}
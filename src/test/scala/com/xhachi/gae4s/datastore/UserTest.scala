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
    User.update(expected)

    val actual = User.get(key)
    assert(actual == expected)
  }

}

case class User(key: Key[User], name: String, webInfo: WebInfo = WebInfo(), createdAt: Date = new Date) extends Entity[User]

case class WebInfo(email: Option[String] = None, twitter: Option[String] = None)

object UserMeta extends EntityMeta[User] {

  val name = StringProperty("name")
  val webInfo = SerializableProperty[WebInfo]("webInfo")
  val createAt = DateProperty("createAt")

  override def properties: Seq[Property[_]] = createAt :: Nil

  override def fromLLEntity(entity: datastore.Entity): User = {
    User(
      entity.getKey,
      name.fromStore(entity),
      webInfo.fromStore(entity),
      createAt.fromStore(entity)
    )
  }

  override def toLLEntity(entity: User): datastore.Entity = {
    implicit val e = createLLEntity(entity)
    name.toStore(entity.name)
    createAt.toStore(entity.createdAt)
    webInfo.toStore(entity.webInfo)
    e
  }

}

object User extends Storable with Queryable with Mutable {
  type E = User
  val datastore = Datastore
  implicit val meta = UserMeta
}


trait Storable {
  type E <: Entity[E]
  def datastore: Datastore
  implicit def meta: EntityMeta[E]

  def get(key: Key[E]) = datastore.get(key)
  def create(e: E) = datastore.create(e)
}

trait Queryable extends Storable {
  def query: Query[E] = Query[E](meta, datastore)
}

trait Mutable extends Storable {
  def update(e: E) = datastore.update(e)
}
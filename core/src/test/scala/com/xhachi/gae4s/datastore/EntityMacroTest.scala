package com.xhachi.gae4s.datastore

import com.google.appengine.api.datastore.Query.SortDirection
import com.google.appengine.api.datastore.{Key => LLKey}
import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig
import com.xhachi.gae4s.tests.AppEngineTestSuite
import org.scalatest.FunSuite


class EntityMacroTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  test("UserのMetaが生成されること") {

    implicit val meta = EntityMeta.createMeta[User]

    assert(meta.entityType == classOf[com.xhachi.gae4s.datastore.User])
    assert(meta.kind == "com.xhachi.gae4s.datastore.User")
    assert(meta.properties.size == 14)
    assert(meta.properties.exists(_.name == "twitter"))

    val key = Datastore.allocateKey[User]
    assert(key.kind == "com.xhachi.gae4s.datastore.User")

    val entity = meta.createEntity(key)
    assert(entity != null)
    assert(entity.key == key)
    entity.name = "Roto"


    val lle = meta.toLLEntity(entity)
    assert(lle.getProperties.size() == 14)

    val n = lle.getProperty("name")
    assert(n == "Roto")

    assert(!lle.isUnindexedProperty("twitter"))

  }

  test("UserInfoのMetaが生成されること") {

    implicit val meta = EntityMeta.createMeta[UserInfo]

    assert(meta.entityType == classOf[com.xhachi.gae4s.datastore.UserInfo])
    assert(meta.kind == "com.xhachi.gae4s.datastore.UserInfo")
    assert(meta.properties.size == 5)
    assert(meta.property("loggedIn").isDefined)
    assert(meta.property("durationFromLastLoginDate").isEmpty)

    assert(meta.property("role").isDefined)

    val s = Datastore.query[UserInfo].sort(_.lastLoginDate)
    assert(s.sorts.size == 1)
    assert(s.sorts.head.name == "lastLoginDate")
    assert(s.sorts.head.direction == SortDirection.ASCENDING)

    val key = Datastore.createKey(Datastore.createKey[User](1), 1)
    val e = meta.createEntity(key)
    e.role = UserRoles.Admin
    e.role2 = UserRoles.User
    Datastore.put(e)

    val got = Datastore.get(key)
    assert(got.role == UserRoles.Admin)
    assert(got.role2 == UserRoles.User)
  }
}

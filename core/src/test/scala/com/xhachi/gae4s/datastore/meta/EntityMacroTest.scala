package com.xhachi.gae4s.datastore.meta

import com.google.appengine.api.datastore.{Key => LLKey}
import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig
import com.xhachi.gae4s.datastore._
import com.xhachi.gae4s.tests.AppEngineTestSuite
import org.scalatest.FunSuite


class EntityMacroTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  test("UserのMetaが生成されること") {

    implicit val meta = EntityMeta.createMeta[User]

    assert(meta.entityType == classOf[com.xhachi.gae4s.datastore.User])
    assert(meta.kind == "com.xhachi.gae4s.datastore.User")
    assert(meta.properties.size == 5)

    val key = Datastore.allocateKey[User]
    assert(key.kind == "com.xhachi.gae4s.datastore.User")

    val entity = meta.createEntity(key)
    assert(entity != null)
    assert(entity.key == key)
    entity.name = "Roto"


    val lle = meta.toLLEntity(entity)
    assert(lle.getProperties.size() == 5)

    val n = lle.getProperty("name")
    assert(n == "Roto")
  }
}



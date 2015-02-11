package com.xhachi.gae4s.datastore

import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig
import com.xhachi.gae4s.tests.AppEngineTestSuite
import org.scalatest.FunSuite


class EntityMetaTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  implicit val meta = EntityMeta.createMeta[User]


  test("UserをcreateしてtoStringできること") {
    val e = meta.createEntity(Datastore.allocateKey)
    val string = meta.toString(e)
    assert(string == "com.xhachi.gae4s.datastore.User(key:com.xhachi.gae4s.datastore.User(1), name:, height:0, weight:0, mobilePhone:None, webInfo:WebInfo(None,None), deleted:false, spouse:None, spouse1:None, spouse2:null, spouse3:null, twitter:None, updatedAt:null, createdAt:null, version:0)")
  }

  test("Userをcreateして保存してtoStringできること") {
    val create = meta.createEntity(Datastore.allocateKey)
    create.name = "Taro"
    Datastore.create(create)

    val created = Datastore.get(create.key)
    val string = meta.toString(created)
    // createdAtが現在時刻になるのでコメントアウト
    // assert(string == "com.xhachi.gae4s.datastore.User(key:com.xhachi.gae4s.datastore.User(1), name:Taro, height:0, weight:0, mobilePhone:None, webInfo:WebInfo(None,None), deleted:false, spouse:None, spouse1:None, spouse2:null, spouse3:null, twitter:None, updatedAt:Wed Feb 11 11:13:08 UTC 2015, createdAt:Wed Feb 11 11:13:08 UTC 2015, version:1")
  }


}



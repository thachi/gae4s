package com.xhachi.gae4s.datastore

import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig
import com.xhachi.gae4s.tests.AppEngineTestSuite
import org.scalatest.FunSuite


class QueryTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  implicit val meta = new UserMeta

  test("User#nameでフィルタできること") {
    val q = Datastore.query[User, UserMeta].filter(_.name #== "a")
    assert(q != null)
  }

  test("User#nameでソートできること") {
    val q = Datastore.query[User, UserMeta].sort(_.name.asc)
    assert(q.sorts.nonEmpty)
  }

  test("User#nameでmacroフィルタできること") {
    val q = Datastore.query[User, UserMeta]
    assert(q != null)


    val f = q.mF(_.name == "a")
    assert(f != null)
  }

  test("User#nameでmacroソートできること") {
    val q = Datastore.query[User, UserMeta]
    assert(q != null)
    assert(q.sorts.isEmpty)


    val q2 = q.mSortBy(_.name)
    assert(q2.sorts.size == 1)
  }

}



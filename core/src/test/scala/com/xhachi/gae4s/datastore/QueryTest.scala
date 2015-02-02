package com.xhachi.gae4s.datastore

import com.google.appengine.api.datastore.Query.{FilterOperator, FilterPredicate => LLFilterPredicate, SortDirection}
import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig
import com.xhachi.gae4s.tests.AppEngineTestSuite
import org.scalatest.{FunSuite, Matchers}

class QueryTest extends FunSuite with AppEngineTestSuite with Matchers {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  test("User#heightで==のフィルタができること") {
    val q = Datastore.query[User].filter(_.height == 1)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == FilterOperator.EQUAL)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "height")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].value == 1)
  }

  test("User#heightで==のフィルタができること2") {
    val i = 2
    val q = Datastore.query[User].filter(_.height == i)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "height")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == FilterOperator.EQUAL)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].value == 2)
  }

  test("User#heightで>=のフィルタができること") {
    val q = Datastore.query[User].filter(_.height >= 2)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == FilterOperator.GREATER_THAN_OR_EQUAL)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "height")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].value == 2)
  }

  test("User#heightで>のフィルタができること") {
    val q = Datastore.query[User].filter(_.height > 3)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == FilterOperator.GREATER_THAN)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "height")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].value == 3)
  }

  test("User#heightで<=のフィルタができること") {
    val q = Datastore.query[User].filter(_.height <= 4)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == FilterOperator.LESS_THAN_OR_EQUAL)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "height")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].value == 4)
  }

  test("User#heightで<のフィルタができること") {
    val q = Datastore.query[User].filter(_.height < 5)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == FilterOperator.LESS_THAN)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "height")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].value == 5)
  }

  test("User#heightで!=のフィルタができること") {
    val q = Datastore.query[User].filter(_.height != 6)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == FilterOperator.NOT_EQUAL)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "height")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].value == 6)
  }

  test("User#deletedで==trueのフィルタができること") {
    val q = Datastore.query[User].filter(_.deleted == true)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == FilterOperator.EQUAL)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "deleted")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].value == true)
  }

  test("User#deletedで==falseのフィルタができること") {
    val q = Datastore.query[User].filter(_.deleted == false)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == FilterOperator.EQUAL)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "deleted")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].value == false)
  }

  ignore("User#deletedで!のフィルタができること") {
    //    val q = Datastore.query[User].filter(!_.deleted)
    //    assert(q != null)
    //    assert(q.filterOption.isDefined)
    //    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    //    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == FilterOperator.EQUAL)
    //    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "deleted")
    //    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].value == false)
  }

  test("User#spouseでNoneフィルタができること") {

    val meta = EntityMeta.createMeta[User]

    val q = Datastore.query[User].filter(_.spouse == None)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == FilterOperator.EQUAL)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "spouse")

    val value = q.filterOption.get.asInstanceOf[FilterPredicate[_]].toLLFilter.asInstanceOf[LLFilterPredicate].getValue
    value should be (null)
  }

  test("User#spouseで非Noneフィルタができること") {
    val q = Datastore.query[User].filter(_.spouse != None)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == FilterOperator.NOT_EQUAL)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "spouse")

    val value = q.filterOption.get.asInstanceOf[FilterPredicate[_]].toLLFilter.asInstanceOf[LLFilterPredicate].getValue
    value should be (null)
  }

  test("User#spouseでnullのフィルタができること") {
    val q = Datastore.query[User].filter(_.spouse == null)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == FilterOperator.EQUAL)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "spouse")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].value == null)
  }

  test("User#spouseで非nullのフィルタができること") {
    val q = Datastore.query[User].filter(_.spouse != null)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == FilterOperator.NOT_EQUAL)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "spouse")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].value == null)
  }

  test("User#heightでソートできること") {
    val q = Datastore.query[User].sort(_.height)
    val s = q.sorts
    assert(s.nonEmpty)
    assert(s.head.name == "height")
    assert(s.head.direction == SortDirection.ASCENDING)
  }

  test("User#heightで逆順ソートできること") {
    val q = Datastore.query[User].sortDesc(_.height)
    val s = q.sorts
    assert(s.nonEmpty)
    assert(s.head.name == "height")
    assert(s.head.direction == SortDirection.DESCENDING)
  }
  test("User#twitterでソートできること") {
    val q = Datastore.query[User].sort(_.twitter)
    val s = q.sorts
    assert(s.nonEmpty)
    assert(s.head.name == "twitter")
    assert(s.head.direction == SortDirection.ASCENDING)
  }

  test("User#twitterで逆順ソートできること") {
    val q = Datastore.query[User].sortDesc(_.twitter)
    val s = q.sorts
    assert(s.nonEmpty)
    assert(s.head.name == "twitter")
    assert(s.head.direction == SortDirection.DESCENDING)
  }


}




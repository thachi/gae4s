package com.xhachi.gae4s.datastore

import com.google.appengine.api.datastore.Query.{FilterPredicate => LLFilterPredicate}
import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig
import com.xhachi.gae4s.tests.AppEngineTestSuite
import org.scalatest.{FunSuite, Matchers}

class QueryTest extends FunSuite with AppEngineTestSuite with Matchers {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  test("User#heightで==のフィルタができること") {
    val q = Query[User].filter(_.height == 1)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.Equal)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].property.name == "height")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].values == Seq(1))
  }

  test("User#heightで==のフィルタができること2") {
    val i = 2
    val q = Query[User].filter(_.height == i)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].property.name == "height")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.Equal)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].values == Seq(2))
  }

  test("User#heightで>=のフィルタができること") {
    val q = Query[User].filter(_.height >= 2)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.GreaterThanOrEqual)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].property.name == "height")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].values == Seq(2))
  }

  test("User#heightで>のフィルタができること") {
    val q = Query[User].filter(_.height > 3)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.GreaterThan)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].property.name == "height")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].values == Seq(3))
  }

  test("User#heightで<=のフィルタができること") {
    val q = Query[User].filter(_.height <= 4)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.LessThanOrEqual)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].property.name == "height")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].values == Seq(4))
  }

  test("User#heightで<のフィルタができること") {
    val q = Query[User].filter(_.height < 5)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.LessThan)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].property.name == "height")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].values == Seq(5))
  }

  test("User#heightで!=のフィルタができること") {
    val q = Query[User].filter(_.height != 6)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.NotEqual)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].property.name == "height")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].values == Seq(6))
  }

  test("User#deletedで==trueのフィルタができること") {
    val q = Query[User].filter(_.deleted == true)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.Equal)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].property.name == "deleted")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].values == Seq(true))
  }

  test("User#deletedで==falseのフィルタができること") {
    val q = Query[User].filter(_.deleted == false)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.Equal)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].property.name == "deleted")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].values == Seq(false))
  }

  ignore("User#deletedで!のフィルタができること") {
    //    val q = Query[User].filter(!_.deleted)
    //    assert(q != null)
    //    assert(q.filterOption.isDefined)
    //    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    //    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.EQUAL)
    //    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].property.name == "deleted")
    //    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].values == false)
  }

  test("User#spouseでNoneフィルタができること") {

    val q = Query[User].filter(_.spouse == None)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.Equal)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].property.name == "spouse")

    //TODO: ちゃんとテストできてる？

    val f = Datastore._toLLFilter[User](q.filterOption.get.asInstanceOf[FilterPredicate[_]])
    val value = f.asInstanceOf[LLFilterPredicate].getValue
    assert(value == null)
  }

  test("User#spouseで非Noneフィルタができること") {
    val q = Query[User].filter(_.spouse != None)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.NotEqual)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].property.name == "spouse")

    val f = Datastore._toLLFilter[User](q.filterOption.get.asInstanceOf[FilterPredicate[_]])
    val value = f.asInstanceOf[LLFilterPredicate].getValue
    assert(value == null)
  }

  test("User#spouseでnullのフィルタができること") {
    val q = Query[User].filter(_.spouse == null)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.Equal)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].property.name == "spouse")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].values.head == null)
  }

  test("User#spouseで非nullのフィルタができること") {
    val q = Query[User].filter(_.spouse != null)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.NotEqual)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].property.name == "spouse")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].values.head == null)
  }

  test("User#heightでソートできること") {
    val q = Query[User].sort(_.height)
    val s = q.sorts
    assert(s.nonEmpty)
    assert(s.head.name == "height")
    assert(s.head.direction == Sort.Direction.Ascending)
  }

  test("User#heightで逆順ソートできること") {
    val q = Query[User].sortDesc(_.height)
    val s = q.sorts
    assert(s.nonEmpty)
    assert(s.head.name == "height")
    assert(s.head.direction == Sort.Direction.Descending)
  }
  test("User#twitterでソートできること") {
    val q = Query[User].sort(_.twitter)
    val s = q.sorts
    assert(s.nonEmpty)
    assert(s.head.name == "twitter")
    assert(s.head.direction == Sort.Direction.Ascending)
  }

  test("User#twitterで逆順ソートできること") {
    val q = Query[User].sortDesc(_.twitter)
    val s = q.sorts
    assert(s.nonEmpty)
    assert(s.head.name == "twitter")
    assert(s.head.direction == Sort.Direction.Descending)
  }


}




package com.xhachi.gae4s.datastore

import com.google.appengine.api.datastore.Query.{FilterPredicate => LLFilterPredicate}
import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig
import com.xhachi.gae4s.tests.AppEngineTestSuite
import org.scalatest.{FunSuite, Matchers}

class QueryTest extends FunSuite with AppEngineTestSuite with Matchers {

  import Query.Implicits._

  override def _localServiceTestConfigs = new LocalDatastoreServiceTestConfig :: super._localServiceTestConfigs

  test("User#heightで==のフィルタができること") {
    val q = Query("user").filter("height" === 1)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.Equal)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "height")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].values == Seq(1))
  }

  test("User#heightで==のフィルタができること2") {
    val i = 2
    val q = Query("user").filter("height" === i)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "height")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.Equal)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].values == Seq(2))
  }

  test("User#heightで>=のフィルタができること") {
    val q = Query("user").filter("height" >= 2)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.GreaterThanOrEqual)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "height")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].values == Seq(2))
  }

  test("User#heightで>のフィルタができること") {
    val q = Query("user").filter("height" > 3)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.GreaterThan)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "height")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].values == Seq(3))
  }

  test("User#heightで<=のフィルタができること") {
    val q = Query("user").filter("height" <= 4)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.LessThanOrEqual)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "height")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].values == Seq(4))
  }

  test("User#heightで<のフィルタができること") {
    val q = Query("user").filter("height" < 5)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.LessThan)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "height")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].values == Seq(5))
  }

  test("User#heightで!=のフィルタができること") {
    val q = Query("user").filter("height" !== 6)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.NotEqual)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "height")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].values == Seq(6))
  }

  test("User#deletedで==trueのフィルタができること") {
    val q = Query("user").filter("deleted" === true)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.Equal)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "deleted")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].values == Seq(true))
  }

  test("User#deletedで==falseのフィルタができること") {
    val q = Query("user").filter("deleted" === false)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.Equal)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "deleted")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].values == Seq(false))
  }

  test("User#deletedで!のフィルタができること") {
    val q = Query("user").filter(!f("deleted"))
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.Equal)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "deleted")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].values == Seq(false))
  }

  test("User#spouseでNoneフィルタができること") {

    val q = Query("user").filter(f("spouse") === null)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.Equal)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "spouse")

    //TODO: ちゃんとテストできてる？

    val filter: FilterPredicate[_] = q.filterOption.get.asInstanceOf[FilterPredicate[_]]
    val f1 = Datastore._toLLFilter(filter)
    val value = f1.asInstanceOf[LLFilterPredicate].getValue
    assert(value == null)
  }

  test("User#spouseで非Noneフィルタができること") {
    val q = Query("user").filter("spouse" !== null)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.NotEqual)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "spouse")

    val filter: FilterPredicate[_] = q.filterOption.get.asInstanceOf[FilterPredicate[_]]
    val f1 = Datastore._toLLFilter(filter)
    val value = f1.asInstanceOf[LLFilterPredicate].getValue
    assert(value == null)
  }

  test("User#spouseでnullのフィルタができること") {
    val q = Query("user").filter(f"spouse" === null)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.Equal)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "spouse")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].values.head == null)
  }

  test("User#spouseで非nullのフィルタができること") {
    val q = Query("user").filter("spouse" !== null)
    assert(q != null)
    assert(q.filterOption.isDefined)
    assert(q.filterOption.get.isInstanceOf[FilterPredicate[_]])
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].operator == Filter.NotEqual)
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].name == "spouse")
    assert(q.filterOption.get.asInstanceOf[FilterPredicate[_]].values.head == null)
  }

  test("User#heightでソートできること") {
    val q: Query = Query("user").sort(s("height").asc)
    val s1 = q.sorts
    assert(s1.nonEmpty)
    assert(s1.head.name == "height")
    assert(s1.head.direction == Sort.Direction.Ascending)
  }

  test("User#heightで逆順ソートできること") {
    val q: Query = Query("user").sort(s("height").desc)
    val s1 = q.sorts
    assert(s1.nonEmpty)
    assert(s1.head.name == "height")
    assert(s1.head.direction == Sort.Direction.Descending)
  }
  test("User#twitterでソートできること") {
    val q: Query = Query("user").sort(s("twitter"))
    val s1 = q.sorts
    assert(s1.nonEmpty)
    assert(s1.head.name == "twitter")
    assert(s1.head.direction == Sort.Direction.Ascending)
  }

  test("User#twitterで逆順ソートできること") {
    val q: Query = Query("user").sort(s("twitter").desc)
    val s1 = q.sorts
    assert(s1.nonEmpty)
    assert(s1.head.name == "twitter")
    assert(s1.head.direction == Sort.Direction.Descending)
  }


}




package com.xhachi.gae4s.datastore

import com.xhachi.gae4s.tests.AppEngineTestSuite
import org.scalatest.FunSuite


class EntityMetaTest extends FunSuite with AppEngineTestSuite {


  test("kindが正しいか") {
    val kind = EntityMeta.entityClassToKindStrategy.toKind(this.getClass)
    assert(kind == "com.xhachi.gae4s.datastore.EntityMetaTest")
  }

  test("strategyをしていしてkindが正しいか1") {
    EntityMeta.entityClassToKindStrategy = new EntityClassToKindStrategy {
      override def toKind(c: Class[_]): String = "hoge"
    }
    val kind = EntityMeta.entityClassToKindStrategy.toKind(this.getClass)
    assert(kind == "hoge")
  }

  test("SimpleNameStrategyでkindが正しいか") {
    EntityMeta.entityClassToKindStrategy = EntityClassToKindStrategy.SimpleNameStrategy
    val kind = EntityMeta.entityClassToKindStrategy.toKind(this.getClass)
    assert(kind == "EntityMetaTest")
  }
  test("ShortPackageStrategyでkindが正しいか") {
    EntityMeta.entityClassToKindStrategy = EntityClassToKindStrategy.ShortPackageStrategy
    val kind = EntityMeta.entityClassToKindStrategy.toKind(this.getClass)
    assert(kind == "c.x.g.d.EntityMetaTest")
  }

  test("strategyを戻してkindが正しいか") {
    EntityMeta.entityClassToKindStrategy = EntityClassToKindStrategy.SimpleNameStrategy
    val kind1 = EntityMeta.entityClassToKindStrategy.toKind(this.getClass)
    assert(kind1 == "EntityMetaTest")

    EntityMeta.entityClassToKindStrategy = EntityClassToKindStrategy.ClassNameStrategy
    val kind2 = EntityMeta.entityClassToKindStrategy.toKind(this.getClass)
    assert(kind2 == "com.xhachi.gae4s.datastore.EntityMetaTest")
  }


}

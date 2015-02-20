package com.xhachi.gae4s.datastore

import com.xhachi.gae4s.datastore.meta.property
import org.scalatest.FunSuite


class EntityPropertySequenceTest extends FunSuite {


  test("EntityPropertySequenceTestEntity1のプロパティが想定の順序になること") {

    val meta = EntityMeta.createMeta[EntityPropertySequenceTestEntity1]
    val names = meta.properties.map(_.name)
    assert(names == Seq("trait1", "trait2", "sample1", "sample2", "trait_x", "updatedAt", "createdAt", "version"))
  }

  test("EntityPropertySequenceTestEntity2のプロパティが想定の順序になること") {

    val meta = EntityMeta.createMeta[EntityPropertySequenceTestEntity2]
    val names = meta.properties.map(_.name)

    assert(names == Seq("sample1", "sample2", "trait2", "trait_x", "trait1", "updatedAt", "createdAt", "version"))
  }
}


class EntityPropertySequenceTestEntity1(val key: Key[EntityPropertySequenceTestEntity1])
  extends Entity[EntityPropertySequenceTestEntity1]
  with EntityPropertySequenceTestTrait1
  with EntityPropertySequenceTestTrait2
  with CreatedAt with UpdatedAt with Version {

  var sample1: String = ""
  var sample2: String = ""
}


class EntityPropertySequenceTestEntity2(val key: Key[EntityPropertySequenceTestEntity2])
  extends Entity[EntityPropertySequenceTestEntity2]
  with EntityPropertySequenceTestTrait1
  with EntityPropertySequenceTestTrait2
  with CreatedAt with UpdatedAt with Version {

  def sample1: String = ""
  def sample2: String = ""

  @property(order = 5999)
  override def trait2 = "2"

  @property(order = 6001)
  override def trait1 = "2"
}

trait EntityPropertySequenceTestTrait1 {

  @property(order = 1)
  def trait1: String = ""

  @property
  def trait_x: String = ""
}


trait EntityPropertySequenceTestTrait2 {

  @property(order = 2)
  def trait2: String = ""
}

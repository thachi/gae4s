package com.xhachi.gae4s.datastore

import scala.reflect.ClassTag


abstract class DataEntity[E <: DataEntity[E, D], D <: AnyRef : ClassTag] extends Entity[E] {
  var data: D
}

abstract class DataEntityMeta[E <: DataEntity[E, D] : ClassTag, D <: AnyRef : Manifest] extends EntityMeta[E] {

  val data: JsonProperty[D] = new JsonProperty[D]("data")

  addApplyToLLEntity {
    (from: Entity, to: LLEntity) => data.setToStore(from.data)(to)
  }

  addApplyFromLLEntity {
    (from: LLEntity, to: Entity) => to.data = data.getFromStore(from)
  }
}

abstract class DataSeqEntity[E <: DataSeqEntity[E, D], D <: AnyRef : ClassTag] extends Entity[E] {
  var data: Seq[D] = Nil
}

abstract class DataSeqEntityMeta[E <: DataSeqEntity[E, D] : ClassTag, D <: AnyRef : Manifest] extends EntityMeta[E] {

  val data: JsonProperty[Seq[D]] = new JsonProperty[Seq[D]]("data")

  addApplyToLLEntity {
    (from: Entity, to: LLEntity) => data.setToStore(from.data)(to)
  }

  addApplyFromLLEntity {
    (from: LLEntity, to: Entity) => to.data = data.getFromStore(from)
  }
}

package com.xhachi.gae4s.datastore

import scala.reflect.ClassTag


abstract class DataEntity[E <: DataEntity[E, D], D <: AnyRef : ClassTag] extends Entity[E] {
  var data: D
}

abstract class DataEntityMeta[E <: DataEntity[E, D] : ClassTag, D <: AnyRef : Manifest] extends EntityMeta[E] {
  //  type Entity <: DataEntity[E, D]

  val data: JsonProperty[D] = new JsonProperty[D]("data")

  addApplyToLLEntity {
    (from: Entity, to: LLEntity) => data.setToStore(from.data)(to)
  }

  addApplyFromLLEntity {
    (from: LLEntity, to: Entity) => to.data = data.getFromStore(from)
  }
}

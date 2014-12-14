package com.xhachi.gae4s.datastore

import scala.reflect.ClassTag


abstract class JsonDataEntity[E <: JsonDataEntity[E, D], D <: AnyRef : ClassTag] extends Entity[E] {
  var data: D
}

abstract class JsonDataEntityMeta[E <: JsonDataEntity[E, D] : ClassTag, D <: AnyRef : Manifest] extends EntityMeta[E] {

  val data = new JsonProperty[D]("data")

  override def properties = super.properties ++ Seq(data)

  addApplyToLLEntity {
    (from: Entity, to: LLEntity) => data.setValueToLLEntity(to)(from.data)
  }

  addApplyFromLLEntity {
    (from: LLEntity, to: Entity) => to.data = data.getValueFromLLEntity(from)
  }
}

abstract class SerializableDataEntity[E <: SerializableDataEntity[E, D], D <: Serializable : ClassTag] extends Entity[E] {
  var data: D
}

abstract class SerializableDataEntityMeta[E <: SerializableDataEntity[E, D] : ClassTag, D <: Serializable : Manifest] extends EntityMeta[E] {

  val data = new SerializableProperty[D]("data")

  override def properties = super.properties ++ Seq(data)

  addApplyToLLEntity {
    (from: Entity, to: LLEntity) => data.setValueToLLEntity(to)(from.data)
  }

  addApplyFromLLEntity {
    (from: LLEntity, to: Entity) => to.data = data.getValueFromLLEntity(from)
  }
}


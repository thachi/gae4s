package com.xhachi.gae4s.datastore

import scala.reflect.ClassTag


abstract class DataEntity[E <: DataEntity[E, D], D <: AnyRef : ClassTag] extends Entity[E] {
  var data: D
}

abstract class DataMeta[E <: DataEntity[E, D] : ClassTag, D <: AnyRef : Manifest] extends EntityMeta[E] {
  //  type Entity <: DataEntity[E, D]

  val data: JsonProperty[D] = new JsonProperty[D]("data")

  addApplyToLLEntity {
    (from: Entity, to: LLEntity) => data.setToStore(from.data)(to)
  }

  addApplyFromLLEntity {
    (from: LLEntity, to: Entity) => to.data = data.getFromStore(from)
  }
}

abstract class MutableDataEntity[E <: MutableDataEntity[E, D], D <: AnyRef : Manifest] extends DataEntity[E, D] with CreatedAt

abstract class MutableDataEntityMeta[E <: MutableDataEntity[E, D] : ClassTag, D <: AnyRef : Manifest]
  extends DataMeta[E, D]
  with CreatedAtMeta {
  override type Entity <: MutableDataEntity[E, D]
}

abstract class ImmutableDataEntity[E <: ImmutableDataEntity[E, D], D <: AnyRef : Manifest]
  extends DataEntity[E, D]
  with CreatedAt
  with UpdatedAt
  with Version

abstract class ImmutableDataEntityMeta[E <: ImmutableDataEntity[E, D] : ClassTag, D <: AnyRef : Manifest]
  extends DataMeta[E, D]
  with CreatedAtMeta
  with UpdatedAtMeta
  with VersionMeta {
  override type Entity <: ImmutableDataEntity[E, D]
}

//
//abstract class DataEntity[D <: AnyRef : ClassTag]
//  extends Entity[DataEntity[D]]
//  with Data[D]
//  with CreatedAt
//  with UpdatedAt
//  with Version
//
//abstract class DataEntityStore[E <: DataEntityStore[E, D], D <: AnyRef : ClassTag : Manifest] extends EntityStore[E] {
//
//  trait DataEntityMeta[ENTITY]
//    extends EntityMeta[E]
//    with CreatedAtMeta
//    with UpdatedAtMeta
//    with VersionMeta
//    with ApplyProperty {
//
//    final val data = new JsonProperty[D]("data")
//
//    addApplyToLLEntity {
//      (from: Entity, to: LLEntity) =>
//        data.setToStore(from.data)(to)
//    }
//
//    addApplyFromLLEntity {
//      (from: LLEntity, to: Entity) => to.data = data.getFromStore(from)
//    }
//  }
//
//}
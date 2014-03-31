package com.xhachi.gae4s.datastore

import com.google.appengine.api.datastore.{Entity => LLEntity}
import scala.reflect.ClassTag


trait Data[D <: AnyRef] {
  final var data: Option[D] = None
}

class DataEntity[D <: AnyRef : ClassTag](val key: Key[DataEntity[D]])
  extends Entity[DataEntity[D]]
  with Data[D]

class DataEntityStore[D <: AnyRef : ClassTag : Manifest] extends EntityStore[DataEntity[D]] {
  override type ENTITY = DataEntity[D]
  override type META = DataEntityMeta[D]

  override val parentKey: Key[_] = null

  override protected def datastore: Datastore = Datastore

  override protected implicit def meta: META = new DataEntityMeta[D]

  class DataEntityMeta[ENTITY: Manifest] extends EntityMeta[DataEntity[D]] with ApplyProperty {
    override type Entity = DataEntity[D]

    final val data = new OptionProperty(new JsonProperty[D]("data"))

    addApplyToLLEntity {
      (from: Entity, to: LLEntity) =>
        data.setToStore(from.data)(to)
    }

    addApplyFromLLEntity {
      (from: LLEntity, to: Entity) => to.data = data.getFromStore(from)
    }

    override def kind: String = implicitly[ClassTag[D]].runtimeClass.getName

    override protected def createEntity(key: Key[DataEntity[D]]): DataEntity[D] = new DataEntity[D](key)
  }

}
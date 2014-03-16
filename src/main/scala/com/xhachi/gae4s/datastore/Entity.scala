package com.xhachi.gae4s.datastore

import scala.reflect.ClassTag
import com.google.appengine.api.datastore

/*

class DataEntity[D <: Serializable : ClassTag](val key: Key[DataEntity[D]], val data: D) extends Entity[DataEntity[D]] {
  override def kind: String = implicitly[ClassTag[D]].runtimeClass.getSimpleName + "@" + classOf[DataEntity[D]].getSimpleName
}

class DataEntityMeta[D <: Serializable : ClassTag] extends EntityMeta[DataEntity[D]] {

  val data = SerializableProperty[D]("data")

  val properties: Seq[Property[_]] = data :: Nil

  override def fromLLEntity(entity: datastore.Entity): DataEntity[D] = {
    new DataEntity(
      Key[DataEntity[D]](entity.getKey),
      data.fromStoreProperty(entity.getProperty("data"))
    )
  }

  override def toLLEntity(entity: DataEntity[D]): datastore.Entity = {
    val e = createLLEntity(entity)
    e.setProperty("data", data.toStoreProperty(entity.data))
    e
  }
}

object DataEntity {

  implicit val meta = new DataEntityMeta[DataEntity[_]]

  def apply[D <: Serializable: ClassTag](data: D) = new DataEntity(Datastore.allocateKey[DataEntity[D]](), data)

  def apply[D <: Serializable: ClassTag](key: Key[DataEntity[D]], data: D) = new DataEntity(key, data)

  def apply[D <: Serializable: ClassTag](name: String, data: D) = new DataEntity(Datastore.createKey[DataEntity[D]](name), data)

  def apply[D <: Serializable: ClassTag](id: Long, data: D) = new DataEntity(Datastore.createKey[DataEntity[D]](id), data)

  def create[D <: Serializable: ClassTag](entity: DataEntity[D]) = Datastore.put(entity)

}
*/
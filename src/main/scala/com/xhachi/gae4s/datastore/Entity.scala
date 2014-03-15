package com.xhachi.gae4s.datastore

import scala.reflect.ClassTag
import com.google.appengine.api.datastore
import com.google.appengine.api.datastore.Blob
import scala.reflect.runtime.universe._


class DataEntity[D <: Serializable : TypeTag: ClassTag](val key: Key[DataEntity[D]], val data: D) extends Entity[DataEntity[D]] {
  override def kind: String = implicitly[ClassTag[D]].runtimeClass.getSimpleName + "@" + classOf[DataEntity[D]].getSimpleName
}

class DataEntityMeta[D <: Serializable : TypeTag: ClassTag] extends EntityMeta[DataEntity[D]] {

  val data = SerializableProperty[D]("data")

  def properties:Seq[Property[_]] = data :: Nil

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

  def apply[D <: Serializable : TypeTag: ClassTag](data: D) = new DataEntity(Datastore.allocateKey[DataEntity[D]](), data)

  def apply[D <: Serializable : TypeTag: ClassTag](key: Key[DataEntity[D]], data: D) = new DataEntity(key, data)

  def apply[D <: Serializable : TypeTag: ClassTag](name: String, data: D) = new DataEntity(Datastore.createKey[DataEntity[D]](name), data)

  def apply[D <: Serializable : TypeTag: ClassTag](id: Long, data: D) = new DataEntity(Datastore.createKey[DataEntity[D]](id), data)

  def create[D <: Serializable : TypeTag: ClassTag](entity: DataEntity[D]) = Datastore.put(entity)

}


trait StoreBase {
  def create[E <: Entity[E] : TypeTag](e: E) = Datastore.put(e)
}

trait Mutable {
  def update[E <: Entity[E] : TypeTag](e: E) = Datastore.put(e)
}





//trait NamedEntity[E <: Entity[E, D], D] extends Entity[E, D] {
//
//  override def key = Datastore.createKey[E](name)
//
//  def name: String
//
//}
//
//
//trait PuttableEntity[E <: MutableEntity[E, D], D] {
//
//  /**
//   * エンティティの存在をチェックせずに保存します。
//   * @return
//   */
//  def put(): Unit = Datastore.put(this)
//}
//
///**
// * 自動的にIDが1のキーがふられる
// *
// */
//trait SingletonEntity[E <: Entity[E, D], D] extends Entity[E, D] {
//  val key: Key[E] = Datastore.createKey[E](1)
//
//  def getSingle: E = Datastore.get(key)
//
//  def getSingleOption: Option[E] = Datastore.getOption(key)
//}
//
///**
// * 更新可能なEntity
// * バージョンナンバーが付与されて楽観的排他制御が働く
// * modification
// */
//trait MutableEntity[E <: Entity[E, D], D] extends Entity[E, D] {
//
//  private var _version = 0L
//
//  def version: Long = _version
//
//  def lastModified: Date
//
//  def update(): Unit = Datastore.getOption(this.key) match {
//    case Some(e: MutableEntity) if e.version == this.version => {
//      _version += 1L
//      Datastore.put(this)
//    }
//    case Some(e) => throw new IllegalStateException("dokkade kousin saretayo.")
//    case None => throw new IllegalStateException("entity not stored.")
//  }
//
//
//}
//
//
//trait AutoAllocatedEntity[E <: Entity[E, D], D] {
//
//  private var internalKey: Key[E]
//
//  def allocate(): Unit = internalKey = Datastore.allocateKey[E]()
//
//}
//
///**
// * 自動でキーが発行されるエンティティ
// */
//trait AutoKeyedEntity {
//
//}
//
///**
// * 祖先を持つエンティティ
// */
//trait AncestorKeyedEntity {
//
//}
//
//
//class HeroEntity(val key: Key[HeroEntity], val data: Hero) extends Entity[HeroEntity, Hero] with PuttableEntity
//
//case class Hero(name: String, level: Int)
//
//
//class SystemSettingEntity(val data: SystemSetting) extends SingletonEntity[SystemSettingEntity, SystemSetting]
//
//case class SystemSetting(val maintenance: Boolean)
//
//
//object Useage {
//  def main(args: Array[String]) = {
//
//    val hero = new HeroEntity(Datastore.createKey("name"), Hero("Roto", 12))
//    hero.create()
//
//    hero.put()
//    hero.put()
//    hero.put()
//  }
//}
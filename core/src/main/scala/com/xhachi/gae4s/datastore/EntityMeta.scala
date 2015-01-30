package com.xhachi.gae4s.datastore

import com.google.appengine.api.datastore.{Key => LLKey, KeyFactory}

import scala.reflect.ClassTag

object EntityMeta {

  import scala.language.experimental.macros

  implicit def createMeta[E <: Entity[E]]: EntityMeta[E] = macro EntityMacro.createMeta[E]
}

abstract class EntityMeta[E <: Entity[E] : ClassTag] extends Serializable {

  type EntityType = E

  def ancestorType: Option[Class[_ <: Entity[_]]]

  def entityType: Class[E] = implicitly[ClassTag[E]].runtimeClass.asInstanceOf[Class[E]]

  def kind: String = implicitly[ClassTag[E]].runtimeClass.getName

  val key = new KeyProperty[EntityType]("__key__") with IndexedProperty[Key[EntityType]]

  def properties: Seq[Property[_]] = Seq(key)

  def property(name: String): Option[Property[_]] = properties.find(_.name == name)

  def versionProperty: Option[VersionProperty] = None

  def versionEnabled: Boolean = versionProperty.isDefined

  def version(e: EntityType): Option[Long] = versionProperty.map(_.getValueFromLLEntity(toLLEntity(e)))

  def createEntity(key: Key[EntityType]): EntityType

  final def toEntity(from: com.google.appengine.api.datastore.Entity): EntityType = {
    val to = createEntity(createKey(from.getKey))
    for (p <- properties if p.isInstanceOf[Setter[_, _]]) {
      val value = p.getValueFromLLEntity(from).asInstanceOf[p.PropertyType]
      val setter:Setter[E, p.PropertyType] = p.asInstanceOf[Setter[E, p.PropertyType]]
      setter.setValueToEntity(to, value.asInstanceOf[p.PropertyType])
    }
    to
  }

  final def toLLEntity(from: EntityType): com.google.appengine.api.datastore.Entity = {
    val to = new com.google.appengine.api.datastore.Entity(from.key.key)
    for (p <- properties if p.isInstanceOf[Getter[_, _]]) {
      val getter = p.asInstanceOf[Getter[E, p.PropertyType]]
      val value = getter.getValueFromEntity(from)
      p.asInstanceOf[Property[p.PropertyType]].setValueToLLEntity(to)(value.asInstanceOf[p.PropertyType])
    }
    to
  }

  def createKey(key: LLKey) = Key[EntityType](key)

  def createKeyWithName(name: String) = {
    val key = KeyFactory.createKey(kind, name)
    Key[EntityType](key)
  }

  def createKeyWithId(id: Long) = {
    Key[EntityType](KeyFactory.createKey(kind, id))
  }

  def createKeyWithName(parent: Key[_], name: String) = {
    val key = KeyFactory.createKey(parent.key, kind, name)
    Key[EntityType](key)
  }

  def createKeyWithId(parent: Key[_], id: Long) = {
    val key = KeyFactory.createKey(parent.key, kind, id)
    Key[EntityType](key)
  }

  def fromKeyStrong(keyString: String): Key[EntityType] = {
    val key = Key[EntityType](KeyFactory.stringToKey(keyString))
    assert(key.kind == kind)
    key
  }

  def toKeyStrong(key: Key[_]): String = KeyFactory.keyToString(key.key)

}
package com.xhachi.gae4s.datastore

import com.google.appengine.api.datastore.{KeyFactory, Key => LLKey}

import scala.reflect.ClassTag

object EntityMeta {

  import scala.language.experimental.macros

  implicit def createMeta[E <: Entity[E]]: EntityMeta[E] = macro EntityMacro.createMeta[E]
}

abstract class EntityMeta[E <: Entity[E] : ClassTag] extends Serializable {

  type Entity = E

  def entityType: Class[_] = implicitly[ClassTag[E]].runtimeClass

  def kind: String = implicitly[ClassTag[E]].runtimeClass.getName

  val key = new KeyProperty[Entity]("key")

  def properties: Seq[Property[_]] = Seq(key)

  def property(name: String): Option[Property[_]] = properties.find(_.name == name)

  def versionProperty: Option[VersionProperty] = None

  def versionEnabled: Boolean = versionProperty.isDefined

  def version(e: Entity): Option[Long] = versionProperty.map(_.getValueFromLLEntity(toLLEntity(e)))

  def createEntity(key: Key[Entity]): Entity

  def toEntity(entity: com.google.appengine.api.datastore.Entity): Entity

  def toLLEntity(entity: Entity): com.google.appengine.api.datastore.Entity

  def createKey(key: LLKey) = Key[Entity](key)

  def createKeyWithName(name: String) = {
    val key = KeyFactory.createKey(kind, name)
    Key[Entity](key)
  }

  def createKeyWithId(id: Long) = {
    Key[Entity](KeyFactory.createKey(kind, id))
  }

  def createKeyWithName(parent: Key[_], name: String) = {
    val key = KeyFactory.createKey(parent.key, kind, name)
    Key[Entity](key)
  }

  def createKeyWithId(parent: Key[_], id: Long) = {
    val key = KeyFactory.createKey(parent.key, kind, id)
    Key[Entity](key)
  }

  def fromKeyStrong(keyString: String): Key[Entity] = {
    val key = Key[Entity](KeyFactory.stringToKey(keyString))
    assert(key.kind == kind)
    key
  }

  def toKeyStrong(key: Key[_]): String = KeyFactory.keyToString(key.key)

}
package com.xhachi.gae4s.datastore

import com.google.appengine.api.datastore.{Key => LLKey, KeyFactory}

import scala.reflect.ClassTag


abstract class Entity[E <: Entity[E]] extends Serializable {

  val key: Key[E]

  final def keyOption: Option[Key[E]] = key match {
    case k: Key[E] => Some(k)
    case _ => None
  }

}

/**
 * TODO: ちゃんとしたエラーチェックの仕組みをDatastoreに入れたい
 * @tparam A 親のキー
 */
trait Ancestor[A <: Entity[A]] {
  self: Entity[_] =>

  if (key != null) assert(key.key.getParent != null)

  def parentKey = key.key.getParent match {
    case k: LLKey => Key[A](k)
    case _ => throw new IllegalStateException("this entity has no parent.")
  }
}

trait EntityClassToKindStrategy {
  def toKind(c: Class[_]): String
}

object EntityClassToKindStrategy {
  val ClassNameStrategy = new EntityClassToKindStrategy {
    override def toKind(c: Class[_]): String = c.getName
  }
  val SimpleNameStrategy = new EntityClassToKindStrategy {
    override def toKind(c: Class[_]): String = c.getSimpleName
  }
  val ShortPackageStrategy = new EntityClassToKindStrategy {
    override def toKind(c: Class[_]): String = c.getPackage.getName.split("\\.") match {
      case split if 0 < split.size => split.map(_.substring(0, 1)).mkString("", ".", ".") + c.getSimpleName
      case _ => c.getSimpleName
    }
  }
}

object EntityMeta {
  var entityClassToKindStrategy = EntityClassToKindStrategy.ClassNameStrategy

  import scala.language.experimental.macros

  implicit def createMeta[E <: Entity[E]]: EntityMeta[E] = macro EntityMacro.createMeta[E]
}

abstract class EntityMeta[E <: Entity[E] : ClassTag] extends Serializable {

  type Entity = E

  def entityType: Class[_] = implicitly[ClassTag[E]].runtimeClass

  def kind: String = EntityMeta.entityClassToKindStrategy.toKind(implicitly[ClassTag[E]].runtimeClass)

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
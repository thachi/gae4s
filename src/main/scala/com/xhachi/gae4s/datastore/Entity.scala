package com.xhachi.gae4s.datastore

import scala.reflect.ClassTag
import com.google.appengine.api.datastore.{Key => LLKey, KeyFactory}
import scala.Some


trait Entity[E <: Entity[E]] {

  final def keyOption: Option[Key[E]] = key match {
    case k: Key[E] => Some(k)
    case _ => None
  }

  def key: Key[E]

}

/**
 * TODO: ちゃんとしたエラーチェックの仕組みをDatastoreに入れたい
 * @tparam A
 */
trait Ancestor[A <: Entity[A]] {

  def key: Key[_]

  if (key != null) assert(key.key.getParent != null)

  def parentKey = key.key.getParent match {
    case k: LLKey => new Key[A](k)
    case _ => throw new IllegalStateException("this entity has no parent.")
  }
}

object EntityMeta {
  private var _classToKindStrategy: (Class[_] => String) = (clazz => clazz.getName)

  def setClassToKindStrategy(strategy: (Class[_]) => String) = {
    _classToKindStrategy = strategy
  }

  def classToKindStrategy = _classToKindStrategy
}

abstract class EntityMeta[E <: Entity[E] : ClassTag]
  extends ApplyProperty
  with EntityMetaCreateKeyMethods {

  type Entity = E

  def kind: String = EntityMeta.classToKindStrategy(implicitly[ClassTag[E]].runtimeClass)

  val key = new KeyProperty("key")

  def createEntity(key: Key[E]): E

  def toEntity(entity: com.google.appengine.api.datastore.Entity): Entity = {
    entity match {
      case _: com.google.appengine.api.datastore.Entity =>
        val e = createEntity(createKey(entity.getKey))
        applyFromLLEntity(entity, e)
        e
      case _ => null.asInstanceOf[Entity]
    }
  }

  def toLLEntity(entity: E): com.google.appengine.api.datastore.Entity = {
    entity match {
      case _: E =>
        val e = new LLEntity(entity.key.key)
        applyToLLEntity(entity, e)
        e
      case _ => null.asInstanceOf[com.google.appengine.api.datastore.Entity]
    }
  }
}


sealed private[datastore] trait EntityMetaCreateKeyMethods {
  type Entity

  def kind: String

  def createKey(key: LLKey) = new Key[Entity](key)

  def createKeyWithName(name: String) = {
    val key = KeyFactory.createKey(kind, name)
    new Key[Entity](key)
  }

  def createKeyWithId(id: Long) = {
    new Key[Entity](KeyFactory.createKey(kind, id))
  }

  def createKeyWithName(parent: Key[_], name: String) = {
    val key = KeyFactory.createKey(parent.key, kind, name)
    new Key[Entity](key)
  }

  def createKeyWithId(parent: Key[_], id: Long) = {
    val key = KeyFactory.createKey(parent.key, kind, id)
    new Key[Entity](key)
  }

  def fromKeyStrong(keyString: String): Key[Entity] = {
    //TODO: newを取る
    val key = new Key[Entity](KeyFactory.stringToKey(keyString))
    assert(key.kind == kind)
    key
  }

  def toKeyStrong(key: Key[_]): String = KeyFactory.keyToString(key.key)

}
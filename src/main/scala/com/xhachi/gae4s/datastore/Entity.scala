package com.xhachi.gae4s.datastore

import scala.reflect.ClassTag


trait Entity[E <: Entity[E]] {

  final def keyOption: Option[Key[E]] = key match {
    case k: Key[E] => Some(k)
    case _ => None
  }

  def key: Key[E]

}

abstract class EntityMeta[E <: Entity[E]: ClassTag] extends ApplyProperty {
  type Entity = E

  def kind: String = implicitly[ClassTag[E]].runtimeClass.getName

  val key = new KeyProperty("key")

  private[datastore] def createEntity(key: Key[E]): E

  def toEntity(entity: com.google.appengine.api.datastore.Entity): Entity = {
    val e = createEntity(Key(entity.getKey))
    applyFromLLEntity(entity, e)
    e
  }

  def toLLEntity(entity: E): com.google.appengine.api.datastore.Entity = {
    val e = new LLEntity(entity.key.key)
    applyToLLEntity(entity, e)
    e
  }

}

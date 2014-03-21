package com.xhachi.gae4s.datastore

import com.google.appengine.api.datastore.{Entity => LLEntity}
import java.util.Date


sealed trait Entity[E <: Entity[E]] {

  final def keyOption: Option[Key[E]] = key match {
    case k: Key[E] => Some(k)
    case _ => None
  }

  def key: Key[E]

}

trait RootEntity[E <: Entity[E]] extends Entity[E]

trait LeafEntity[E <: Entity[E]] extends Entity[E] {
  type ParentEntity <: Entity[ParentEntity]
}


trait EntityMeta[E <: Entity[E]] extends ApplyProperty {
  type Entity = E

  def kind: String

  protected def createEntity(entity: LLEntity): E

  def toEntity(entity: LLEntity) : Entity = {
    val e = createEntity(entity)
    applyFromLLEntity(entity, e)
    e
  }

  def toLLEntity(entity: E): LLEntity = {
    val e = new LLEntity(entity.key.key)
    applyToLLEntity(entity, e)
    e
  }


}

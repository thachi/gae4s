package com.xhachi.gae4s.datastore

import com.google.appengine.api.datastore.{Entity => LLEntity}

trait Entity[E <: Entity[E]] {

  final def keyOption: Option[Key[E]] = key match {
    case k: Key[E] => Some(k)
    case _ => None
  }

  def key: Key[E]

}

trait EntityMeta[E <: Entity[E]]  {

  def kind: String

  def toLLEntity(entity: E): LLEntity

  def fromLLEntity(entity: LLEntity): E

  def createLLEntity(entity: E) = entity.keyOption match {
    case Some(k) => new LLEntity(k.key)
    case None => new LLEntity(entity.getClass.getName)
  }

}

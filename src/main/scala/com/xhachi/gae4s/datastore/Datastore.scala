package com.xhachi.gae4s.datastore

import scala.collection.JavaConversions._
import com.google.appengine.api.datastore.{Key => LLKey, Entity => LLEntity, Query => LLQuery, _}

final case class Key[E] private[datastore](private[datastore] val key: LLKey) {

  def kind: String = key.getKind

  def id: Option[Long] = key.getId match {
    case i if 0 < i => Some(i)
    case _ => None
  }

  def name: Option[String] = key.getName match {
    case n: String => Some(n)
    case _ => None
  }

}

trait Entity[E <: Entity[E]] {

  def kind: String = this.getClass.getName

  final def keyOption: Option[Key[E]] = key match {
    case k: Key[E] => Some(k)
    case _ => None
  }

  def key: Key[E]

}

trait EntityMeta[E <: Entity[E]] extends KeyConverter {

  def properties: Seq[Property[_, _]]

  def toLLEntity(entity: E): LLEntity

  def fromLLEntity(entity: LLEntity): E

  def createLLEntity(entity: E) = entity.keyOption match {
    case Some(k) => new LLEntity(k.key)
    case None => new LLEntity(entity.getClass.getName)
  }

}


class Datastore private[datastore](private[datastore] val service: DatastoreService)
  extends KeyConverter
  with QueryConverter {

  def getOption[E <: Entity[E]](key: Key[E])(implicit meta: EntityMeta[E]): Option[E] =
    try {
      Some(meta.fromLLEntity(service.get(key)))
    } catch {
      case e: EntityNotFoundException => None
    }

  def get[E <: Entity[E]](key: Key[E])(implicit meta: EntityMeta[E]): E = getOption(key).get

  def getOrElse[E <: Entity[E]](key: Key[E], default: () => E)(implicit meta: EntityMeta[E]) =
    getOption(key) match {
      case Some(e) => e
      case None => default
    }


  def put[E <: Entity[E]](entity: E)(implicit meta: EntityMeta[E]) = {
    service.put(meta.toLLEntity(entity))
  }

  def create[E <: Entity[E]](entity: E)(implicit meta: EntityMeta[E]): Key[E] = {
    getOption(entity.key) match {
      case Some(e) => throw new IllegalStateException()
      case None => Key[E](put(entity))
    }
  }


  def update[E <: Entity[E]](entity: E)(implicit meta: EntityMeta[E]): Unit = {
    getOption(entity.key) match {
      case Some(e) => put(entity)
      case None => throw new IllegalStateException()
    }
  }

  def query[E <: Entity[E], M <: EntityMeta[E]](implicit meta: M) = Query[E, M](this, meta)

  def count[E <: Entity[E], M <: EntityMeta[E]](query: Query[E, M]): Int = {
    val llQuery = toLLQuery(query, false)
    service.prepare(llQuery).countEntities(FetchOptions.Builder.withLimit(Int.MaxValue))
  }

  def asSeq[E <: Entity[E], M <: EntityMeta[E]](query: Query[E, M]): Seq[E] = {
    val llQuery = toLLQuery(query, false)
    service.prepare(llQuery).asIterable.map(query.meta.fromLLEntity).toSeq
  }

  def asSingle[E <: Entity[E], M <: EntityMeta[E]](query: Query[E, M]): E = {
    val llQuery: LLQuery = toLLQuery(query, false)
    query.meta.fromLLEntity(service.prepare(llQuery).asSingleEntity())
  }

  def asKeySeq[E <: Entity[E], M <: EntityMeta[E]](query: Query[E, M]): Seq[Key[E]] = {
    val llQuery = toLLQuery(query, true)
    service.prepare(llQuery).asIterable.map(e => toKey[E](e.getKey)).toSeq
  }

  def createKey[E <: Entity[E]](name: String): Key[E] = new Key[E](KeyFactory.createKey("kind", name))

  def createKey[E <: Entity[E]](id: Long): Key[E] = new Key[E](KeyFactory.createKey("kind", id))

  def allocateKey[E <: Entity[E]](): Key[E] = new Key[E](service.allocateIds("kind", 1).getStart)

}


object Datastore extends Datastore(DatastoreServiceFactory.getDatastoreService) {
  def apply(service: DatastoreService) = new Datastore(service)
}


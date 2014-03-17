package com.xhachi.gae4s.datastore

import scala.collection.JavaConversions._
import com.google.appengine.api.datastore.{Entity => LLEntity, Query => LLQuery, _}

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


class Datastore private[datastore](private[datastore] val service: DatastoreService)
  extends QueryConverter {

  def getOption[E <: Entity[E]](key: Key[E])(implicit meta: EntityMeta[E]): Option[E] =
    try {
      Some(meta.fromLLEntity(service.get(key.key)))
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
    val e = meta.toLLEntity(entity)
    service.put(e)
  }

  def create[E <: Entity[E]](entity: E)(implicit meta: EntityMeta[E]): Key[E] = {
    getOption(entity.key) match {
      case Some(e) => throw new IllegalStateException("entity was already stored")
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
    val llQuery = toLLQuery(query, keysOnly = false)
    service.prepare(llQuery).countEntities(FetchOptions.Builder.withLimit(Int.MaxValue))
  }

  def asSeq[E <: Entity[E], M <: EntityMeta[E]](query: Query[E, M]): Seq[E] = {
    val llQuery = toLLQuery(query, keysOnly = false)
    service.prepare(llQuery).asIterable.map(query.meta.fromLLEntity).toSeq
  }

  def asSingle[E <: Entity[E], M <: EntityMeta[E]](query: Query[E, M]): E = {
    val llQuery: LLQuery = toLLQuery(query, keysOnly = false)
    query.meta.fromLLEntity(service.prepare(llQuery).asSingleEntity())
  }

  def asKeySeq[E <: Entity[E], M <: EntityMeta[E]](query: Query[E, M]): Seq[Key[E]] = {
    val llQuery = toLLQuery(query, keysOnly = true)
    service.prepare(llQuery).asIterable.map(e => Key[E](e.getKey)).toSeq
  }

  def createKey[E <: Entity[E], M <: EntityMeta[E]](name: String)(implicit meta: M): Key[E] = Key[E](name)

  def createKey[E <: Entity[E], M <: EntityMeta[E]](id: Long)(implicit meta: M): Key[E] = Key[E](id)

  def allocateKey[E <: Entity[E], M <: EntityMeta[E]](implicit meta: M): Key[E] = {

    Key[E](service.allocateIds(meta.kind, 1).getStart.getId)
  }

}


object Datastore extends Datastore(DatastoreServiceFactory.getDatastoreService) {
  def apply(service: DatastoreService) = new Datastore(service)
}


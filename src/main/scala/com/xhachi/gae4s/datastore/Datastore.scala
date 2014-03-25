package com.xhachi.gae4s.datastore

import scala.collection.JavaConversions._
import com.google.appengine.api.datastore.{Entity => LLEntity, Query => LLQuery, _}

class Datastore private[datastore](private[datastore] val service: DatastoreService) {

  def getOption[E <: Entity[E]](key: Key[E])(implicit meta: EntityMeta[E]): Option[E] =
    try {
      Some(meta.toEntity(service.get(key.key)))
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

  def query[E <: Entity[E], M <: EntityMeta[E]](ancestor: Key[_])(implicit meta: M) = Query[E, M](this, meta, Some(ancestor))

  def count[E <: Entity[E], M <: EntityMeta[E]](query: Query[E, M]): Int = {
    val llQuery = query.toLLQuery(keysOnly = false)
    service.prepare(llQuery).countEntities(FetchOptions.Builder.withLimit(Int.MaxValue))
  }

  def asSeq[E <: Entity[E], M <: EntityMeta[E]](query: Query[E, M]): Seq[E] = {
    val llQuery = query.toLLQuery(keysOnly = false)
    service.prepare(llQuery).asIterable.map(query.meta.toEntity(_)).toSeq
  }

  def asSingle[E <: Entity[E], M <: EntityMeta[E]](query: Query[E, M]): E = {
    val llQuery: LLQuery = query.toLLQuery(keysOnly = false)
    query.meta.toEntity(service.prepare(llQuery).asSingleEntity())
  }

  def asKeySeq[E <: Entity[E], M <: EntityMeta[E]](query: Query[E, M]): Seq[Key[E]] = {
    val llQuery = query.toLLQuery(keysOnly = true)
    service.prepare(llQuery).asIterable.map(e => Key[E](e.getKey)).toSeq
  }

  def createKey[E <: Entity[E], M <: EntityMeta[E]](name: String)(implicit meta: M): Key[E] = Key[E](name)

  def createKey[E <: Entity[E], M <: EntityMeta[E]](parent: Key[_], name: String)(implicit meta: M): Key[E] = Key[E](parent, name)

  def createKey[E <: Entity[E], M <: EntityMeta[E]](id: Long)(implicit meta: M): Key[E] = Key[E](id)

  def createKey[E <: Entity[E], M <: EntityMeta[E]](parent: Key[_], id: Long)(implicit meta: M): Key[E] = Key[E](parent, id)

  def allocateKey[E <: Entity[E], M <: EntityMeta[E]](implicit meta: M): Key[E] = {
    Key[E](service.allocateIds(meta.kind, 1).getStart.getId)
  }

  def allocateKey[E <: Entity[E], M <: EntityMeta[E]](parent: Key[_])(implicit meta: M): Key[E] = {
    Key[E](parent, service.allocateIds(meta.kind, 1).getStart.getId)
  }

}


object Datastore extends Datastore(DatastoreServiceFactory.getDatastoreService) {
  def apply(service: DatastoreService) = new Datastore(service)
}


sealed trait EntityStore {
  protected type E <: Entity[E]
  protected type M <: EntityMeta[E]

  protected implicit def meta: M

  protected def datastore: Datastore

  def create(e: E) = datastore.create(e)
}

trait RootEntityStore extends EntityStore {

}

trait LeafEntityStore[P <: Entity[P]] extends EntityStore {
  def parentKey: Key[P]
}

trait KeyedStore extends EntityStore {
  def get(key: Key[E]) = datastore.get(key)
}

trait SingleStore extends IdentifiableKeyStore {

  def createSingleKey: Key[E] = createKey(1)

  def getSingle: E = datastore.get(createSingleKey)
}

trait NamedStore extends KeyedStore {

  def createKey(name: String) = this match {
    case s: RootEntityStore => datastore.createKey[E, M](name)
    case s: LeafEntityStore[_] => datastore.createKey[E, M](s.parentKey, name)
  }
}

trait IdentifiableKeyStore extends KeyedStore {
  def createKey(id: Long) = this match {
    case s: RootEntityStore => datastore.createKey[E, M](id)
    case s: LeafEntityStore[_] => datastore.createKey[E, M](s.parentKey, id)
  }
}

trait AutoAllocateKeyStore extends KeyedStore {
  def allocateKey = this match {
    case s: RootEntityStore => datastore.allocateKey[E, M]
    case s: LeafEntityStore[_] => datastore.allocateKey[E, M](s.parentKey)
  }
}


trait QueryableStore extends EntityStore {
  def query: Query[E, M] =  this match {
    case s: LeafEntityStore[_] if s.parentKey != null => datastore.query[E, M](s.parentKey)
    case _ => datastore.query[E, M]
  }
}

trait UpdatableStore extends EntityStore {
  def update(e: E) = datastore.update(e)
}
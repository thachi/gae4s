package com.xhachi.gae4s.datastore

import com.google.appengine.api.datastore.{Key => LLKey, Entity => LLEntity, Query => LLQuery, _}
import com.google.appengine.api.datastore.Query.{FilterPredicate => LLFilterPredicate, SortDirection => LLSortDirection}
import com.google.appengine.api.datastore.Query.{CompositeFilter => LLCompositeFilter}

import java.io.ObjectInputStream

import scala.collection.JavaConversions._

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._


final case class Key[E: TypeTag] private[datastore](private[datastore] val key: LLKey) {

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

trait EntityMeta[E <: Entity[E]] {

  def properties: Seq[Property[_]]

  def toLLEntity(entity: E): LLEntity

  def fromLLEntity(entity: LLEntity): E

  def createLLEntity(entity: E) = entity.keyOption match {
    case Some(k) => new LLEntity(k.key)
    case None => new LLEntity(entity.getClass.getName)
  }

}


class Datastore private[datastore](private[datastore] val service: DatastoreService)
  extends EntityConverter
  with QueryConverter
  with KeyConverter {

  private def serviceGet(key: LLKey): Option[LLEntity] = {
    try {
      Some(service.get(key))
    }
    catch {
      case e: EntityNotFoundException => None
    }
  }

  def getOption[E <: Entity[E] : TypeTag](key: Key[E]): Option[E] = {

    serviceGet(toLLKey(key)) match {
      case None => None
      case Some(e) => Some(toEntity[E](e))
    }
  }

  def get[E <: Entity[E] : TypeTag](key: Key[E]): E = getOption(key).get

  def getOrElse[E <: Entity[E] : TypeTag](key: Key[E], default: () => E) = {
    getOption(key) match {
      case Some(e) => e
      case None => default
    }
  }

  def put[E <: Entity[E] : TypeTag](entity: E) = service.put(toLLEntity(entity))

  def create[E <: Entity[E] : TypeTag](entity: E): Key[E] = {
    getOption(entity.key) match {
      case Some(e) => throw new IllegalStateException()
      case None => Key[E](put(entity))
    }
  }

  def update[E <: Entity[E] : TypeTag](entity: E): Unit = {
    getOption(entity.key) match {
      case Some(e) => put(entity)
      case None => throw new IllegalStateException()
    }
  }

  def query[E <: Entity[E] : TypeTag] = Query("kind", this)

  def count[E <: Entity[E] : TypeTag](query: Query[E]): Long = {
    val llQuery = toLLQuery(query.copy(keysOnly = false))
    service.prepare(llQuery).countEntities(FetchOptions.Builder.withLimit(Int.MaxValue))
  }

  def asSeq[E <: Entity[E] : TypeTag](query: Query[E]): Seq[E] = {
    val llQuery = toLLQuery(query.copy(keysOnly = false))
    service.prepare(llQuery).asIterable.map(toEntity[E]).toSeq
  }

  def asSingle[E <: Entity[E] : TypeTag](query: Query[E]): E = {
    val llQuery: LLQuery = toLLQuery(query.copy(keysOnly = false))
    toEntity(service.prepare(llQuery).asSingleEntity())
  }

  def asKeySeq[E <: Entity[E] : TypeTag](query: Query[E]): Seq[Key[E]] = {
    val llQuery = toLLQuery(query.copy(keysOnly = true))
    service.prepare(llQuery).asIterable.map(e => toKey[E](e.getKey)).toSeq
  }

  //  def asKeySeq: Seq[Key[E]] =


  //  implicit private def keyToLLKey(key: Key): LLKey = key.key

  def createKey[E <: Entity[E] : TypeTag](name: String): Key[E] = new Key[E](KeyFactory.createKey("kind", name))

  def createKey[E <: Entity[E] : TypeTag](id: Long): Key[E] = new Key[E](KeyFactory.createKey("kind", id))

  def allocateKey[E <: Entity[E] : TypeTag](): Key[E] = new Key[E](service.allocateIds("kind", 1).getStart)

}


object Datastore extends Datastore(DatastoreServiceFactory.getDatastoreService) {
  def apply(service: DatastoreService) = new Datastore(service)
}


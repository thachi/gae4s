package com.xhachi.gae4s.datastore

import java.util.ConcurrentModificationException

import com.google.appengine.api.datastore.{Entity => LLEntity, _}

import scala.collection.JavaConversions._
import scala.language.implicitConversions

/**
 * Class to access Datastore service.
 *
 * @author Takashi Hachinohe
 * @param service the DatastoreService instance
 */
class Datastore private[datastore](private[datastore] val service: DatastoreService)
  extends DatastoreBase
  with DatastoreGetMethods
  with DatastoreGetOptionMethods
  with DatastoreGetListMethods
  with DatastorePutMethods
  with DatastorePutListMethods
  with DatastoreCreateMethods
  with DatastoreCreateListMethods
  with DatastoreUpdateMethods
  with DatastoreUpdateListMethods
  with DatastoreDeleteMethods
  with DatastoreDeleteListMethods
  with DatastoreQueryMethods
  with DatastoreCreateKeyMethods
  with DatastoreTxMethods {

  implicit def toDatastoreQuery[E <: Entity[E]](query: Query[E])(implicit meta: EntityMeta[E]): DatastoreQuery[E] = DatastoreQuery(this, query)
}

/**
 * Object to access default Datastore service.
 *
 * @author Takashi Hachinohe
 */
object Datastore extends Datastore(DatastoreServiceFactory.getDatastoreService) {
  def apply(service: DatastoreService) = new Datastore(service)
}

sealed private[datastore] trait DatastoreBase {

  private[datastore] def service: DatastoreService
}

sealed private[datastore] trait DatastoreGetMethods {
  self: DatastoreBase =>

  def getWithoutTx[E <: Entity[E]](key: Key[E])(implicit meta: EntityMeta[E]): E = getWithTx(null, key)

  def getWithTx[E <: Entity[E]](tx: Transaction, key: Key[E])(implicit meta: EntityMeta[E]): E = meta.toEntity(service.get(tx, key.key))

  def get[E <: Entity[E]](key: Key[E])(implicit meta: EntityMeta[E]): E = meta.toEntity(service.get(key.key))
}

sealed private[datastore] trait DatastoreGetOptionMethods {
  self: DatastoreBase with DatastoreGetMethods =>

  def getOptionWithoutTx[E <: Entity[E]](key: Key[E])(implicit meta: EntityMeta[E]): Option[E] = {
    getOptionWithTx(null, key)
  }

  def getOptionWithTx[E <: Entity[E]](tx: Transaction, key: Key[E])(implicit meta: EntityMeta[E]): Option[E] = try {
    Some(getWithTx(tx, key))
  } catch {
    case e: EntityNotFoundException => None
  }

  def getOption[E <: Entity[E]](key: Key[E])(implicit meta: EntityMeta[E]): Option[E] = try {
    Some(get(key))
  } catch {
    case e: EntityNotFoundException => None
  }
}

sealed private[datastore] trait DatastoreGetListMethods {
  self: DatastoreBase =>

  def getWithoutTx[E <: Entity[E]](keys: Seq[Key[E]])(implicit meta: EntityMeta[E]): Map[Key[E], E] = getWithTx(null, keys)

  def getWithTx[E <: Entity[E]](tx: Transaction, keys: Seq[Key[E]])(implicit meta: EntityMeta[E]): Map[Key[E], E] = {
    val entities = service.get(tx, keys.map(_.key)).map {
      case (k, v) => meta.createKey(k) -> meta.toEntity(v)
    }
    entities.toMap
  }

  def get[E <: Entity[E]](keys: Seq[Key[E]])(implicit meta: EntityMeta[E]): Map[Key[E], E] = {
    val got = service.get(keys.map(_.key))
    val entities = got.map {
      case (k, v) => meta.createKey(k) -> meta.toEntity(v)
    }
    entities.toMap
  }
}

sealed private[datastore] trait DatastoreDeleteMethods {
  self: DatastoreBase =>

  def deleteWithoutTx[E <: Entity[E]](key: Key[E])(implicit meta: EntityMeta[E]): Unit = deleteWithTx(null, key)

  def deleteWithTx[E <: Entity[E]](tx: Transaction, key: Key[E])(implicit meta: EntityMeta[E]): Unit = service.delete(tx, key.key)

  def delete[E <: Entity[E]](key: Key[E])(implicit meta: EntityMeta[E]): Unit = service.delete(key.key)
}

sealed private[datastore] trait DatastoreDeleteListMethods {
  self: DatastoreBase =>

  def deleteWithoutTx[E <: Entity[E]](keys: Seq[Key[E]])(implicit meta: EntityMeta[E]): Unit = deleteWithTx(null, keys)

  def deleteWithTx[E <: Entity[E]](tx: Transaction, keys: Seq[Key[E]])(implicit meta: EntityMeta[E]): Unit = service.delete(tx, keys.map(_.key))

  def delete[E <: Entity[E]](keys: Seq[Key[E]])(implicit meta: EntityMeta[E]): Unit = service.delete(keys.map(_.key))
}

sealed private[datastore] trait DatastorePutMethods extends DatastoreBase {

  def putWithoutTx[E <: Entity[E]](entity: E)(implicit meta: EntityMeta[E]): Key[E] = putWithTx(null, entity)

  def putWithTx[E <: Entity[E]](tx: Transaction, entity: E)(implicit meta: EntityMeta[E]): Key[E] = {
    val e = meta.toLLEntity(entity)
    meta.createKey(service.put(tx, e))
  }

  def put[E <: Entity[E]](entity: E)(implicit meta: EntityMeta[E]): Key[E] = {
    val e = meta.toLLEntity(entity)
    meta.createKey(service.put(e))
  }
}

sealed private[datastore] trait DatastorePutListMethods {
  self: DatastoreBase =>

  def putWithoutTx[E <: Entity[E]](entities: Seq[E])(implicit meta: EntityMeta[E]): Seq[Key[E]] = putWithTx(null, entities)

  def putWithTx[E <: Entity[E]](tx: Transaction, entities: Seq[E])(implicit meta: EntityMeta[E]): Seq[Key[E]] = {
    val e = entities.map(meta.toLLEntity)
    service.put(tx, e).map(k => meta.createKey(k)).toSeq
  }

  def put[E <: Entity[E]](entities: Seq[E])(implicit meta: EntityMeta[E]): Seq[Key[E]] = {
    val e = entities.map(meta.toLLEntity)
    service.put(e).map(k => meta.createKey(k)).toSeq
  }
}

sealed private[datastore] trait DatastoreCreateMethods {
  self: DatastoreBase with DatastorePutMethods with DatastoreGetOptionMethods =>


  def createWithoutTx[E <: Entity[E]](entity: E)(implicit meta: EntityMeta[E]): Key[E] = createWithTx(null, entity)

  def createWithTx[E <: Entity[E]](tx: Transaction, entity: E)(implicit meta: EntityMeta[E]): Key[E] = entity.keyOption match {
    case Some(k) => getOptionWithTx(tx, k) match {
      case Some(e) => throw new ConcurrentModificationException("entity was already stored")
      case None => putWithTx(tx, entity)
    }
    case None => putWithTx(tx, entity)
  }

  def create[E <: Entity[E]](entity: E)(implicit meta: EntityMeta[E]): Key[E] = entity.keyOption match {
    case Some(k) => getOption(k) match {
      case Some(e) => throw new ConcurrentModificationException("entity was already stored")
      case None => put(entity)
    }
    case None => put(entity)
  }
}

sealed private[datastore] trait DatastoreCreateListMethods {
  self: DatastoreBase with DatastorePutListMethods with DatastoreGetListMethods =>

  def createWithoutTx[E <: Entity[E]](entities: Seq[E])(implicit meta: EntityMeta[E]): Seq[Key[E]] = createWithTx(null, entities)

  def createWithTx[E <: Entity[E]](tx: Transaction, entities: Seq[E])(implicit meta: EntityMeta[E]): Seq[Key[E]] = {
    val keys = entities.map(_.key)
    if (getWithTx[E](tx, keys).exists(_._2 == null)) throw new ConcurrentModificationException("entity which is stored already is included")
    putWithTx(tx, entities)
  }

  def create[E <: Entity[E]](entities: Seq[E])(implicit meta: EntityMeta[E]): Seq[Key[E]] = {
    val keys = entities.map(_.key)
    if (get[E](keys).exists(_._2 == null)) throw new ConcurrentModificationException("entity which is stored already is included")
    put(entities)
  }
}

sealed private[datastore] trait DatastoreUpdateMethods {
  self: DatastoreBase with DatastorePutMethods with DatastoreGetOptionMethods =>

  def updateWithoutTx[E <: Entity[E]](entity: E)(implicit meta: EntityMeta[E]): Key[E] = updateWithTx(null, entity)

  def updateWithTx[E <: Entity[E]](tx: Transaction, entity: E)(implicit meta: EntityMeta[E]): Key[E] = getOptionWithTx(tx, entity.key) match {
    case Some(e) if isSameVersion(entity, e) => putWithTx(tx, entity)
    case None => throw new IllegalStateException("entity is not exists in datastore.")
  }

  def update[E <: Entity[E]](entity: E)(implicit meta: EntityMeta[E]): Key[E] = if (meta.versionEnabled) {
    getOption(entity.key) match {
      case Some(e) if isSameVersion(entity, e) => put(entity)
      case Some(e) => throw new ConcurrentModificationException("invalid version property. %s store:%d, stored:%d".format(entity.key, meta.version(entity).get, meta.version(e).get))
      case None => throw new IllegalStateException("entity is not exists in datastore.")
    }
  } else {
    put(entity)
  }

  private def isSameVersion[E <: Entity[E]](entity1: E, entity2: E)(implicit meta: EntityMeta[E]) =
    (meta.version(entity1), meta.version(entity2)) match {
      case (Some(v1), Some(v2)) => v1 == v2
      case _ => throw new IllegalStateException("%s versioning is not enabled.".format(meta.kind))
    }
}

sealed private[datastore] trait DatastoreUpdateListMethods {
  self: DatastoreBase with DatastorePutListMethods with DatastoreGetListMethods =>

  def updateWithoutTx[E <: Entity[E]](entities: Seq[E])(implicit meta: EntityMeta[E]): Seq[Key[E]] = updateWithTx(null, entities)

  def updateWithTx[E <: Entity[E]](tx: Transaction, entities: Seq[E])(implicit meta: EntityMeta[E]): Seq[Key[E]] = {
    val got = get[E](entities.map(_.key)).values.toSeq
    val invalids = getInvalidVersion(entities, got)
    if (invalids.nonEmpty) throw new ConcurrentModificationException("invalid version property.\n" + invalids.mkString("\n"))
    putWithTx(tx, entities)
  }

  def update[E <: Entity[E]](entities: Seq[E])(implicit meta: EntityMeta[E]): Seq[Key[E]] = {
    val got = get[E](entities.map(_.key)).values.toSeq
    val invalids = getInvalidVersion(entities.sortBy(_.key), got.sortBy(_.key))
    if (invalids.nonEmpty) throw new ConcurrentModificationException("invalid version property. " + invalids)
    put(entities)
  }

  private def getInvalidVersion[E <: Entity[E]](entity1: Seq[E], entity2: Seq[E])(implicit meta: EntityMeta[E]): Seq[String] = {
    assert(entity1.size == entity2.size)
    if (meta.versionEnabled) {
      entity1.zip(entity2).filterNot {
        case (e1, e2) =>
          meta.version(e1) == meta.version(e2)
      }.map {
        case (e1, e2) =>
          "%s store:%d, stored:%d".format(e1.asInstanceOf[Entity[_]].key, meta.version(e1).get, meta.version(e2).get)
      }.toSeq
    } else {
      Nil
    }
  }
}

sealed private[datastore] trait DatastoreCreateKeyMethods {
  self: DatastoreBase =>

  def createKey[E <: Entity[E]](name: String)(implicit meta: EntityMeta[E]): Key[E] = meta.createKeyWithName(name)

  def createKey[E <: Entity[E]](parent: Key[_], name: String)(implicit meta: EntityMeta[E]): Key[E] = meta.createKeyWithName(parent, name)

  def createKey[E <: Entity[E]](id: Long)(implicit meta: EntityMeta[E]): Key[E] = meta.createKeyWithId(id)

  def createKey[E <: Entity[E]](parent: Key[_], id: Long)(implicit meta: EntityMeta[E]): Key[E] = meta.createKeyWithId(parent, id)

  def allocateKey[E <: Entity[E]](implicit meta: EntityMeta[E]): Key[E] = {
    meta.createKeyWithId(service.allocateIds(meta.kind, 1).getStart.getId)
  }

  def allocateKeys[E <: Entity[E]](count: Long)(implicit meta: EntityMeta[E]): Seq[Key[E]] = {
    service.allocateIds(meta.kind, count).map(k => meta.createKey(k)).toSeq
  }

  def allocateKey[E <: Entity[E]](parent: Key[_])(implicit meta: EntityMeta[E]): Key[E] = {
    meta.createKey(service.allocateIds(parent.key, meta.kind, 1).getStart)
  }

  def allocateKeys[E <: Entity[E]](parent: Key[_], count: Long)(implicit meta: EntityMeta[E]): Seq[Key[E]] = {
    service.allocateIds(parent.key, meta.kind, count).map(k => meta.createKey(k)).toSeq
  }
}

sealed private[datastore] trait DatastoreQueryMethods {
  self: DatastoreBase =>

  def query[E <: Entity[E]](implicit meta: EntityMeta[E]) = Query[E](meta)

  def query[E <: Entity[E]](ancestor: Key[_])(implicit meta: EntityMeta[E]) = Query[E](meta, Some(ancestor))

  def countWithTx[E <: Entity[E]](tx: Transaction)(implicit meta: EntityMeta[E]): Int = _count(Some(tx), Datastore.query)

  def countWithTx[E <: Entity[E]](tx: Transaction, query: Query[E])(implicit meta: EntityMeta[E]): Int = _count(Some(tx), query)

  def countWithoutTx[E <: Entity[E]](implicit meta: EntityMeta[E]): Int = _count(Some(null), Datastore.query)

  def countWithoutTx[E <: Entity[E]](query: Query[E])(implicit meta: EntityMeta[E]): Int = _count(Some(null), query)

  def count[E <: Entity[E]](implicit meta: EntityMeta[E]): Int = _count(None, Datastore.query)

  def count[E <: Entity[E]](query: Query[E])(implicit meta: EntityMeta[E]): Int = _count(None, query)

  private def _count[E <: Entity[E]](tx: Option[Transaction], query: Query[E])(implicit meta: EntityMeta[E]): Int = {
    val llQuery = query.toLLQuery(keysOnly = false)
    service.prepare(llQuery).countEntities(FetchOptions.Builder.withLimit(Int.MaxValue))
  }

  private def prepare[E <: Entity[E]](tx: Option[Transaction], query: Query[E], keysOnly: Boolean): PreparedQuery = {
    val llQuery = query.toLLQuery(keysOnly = keysOnly)
    tx match {
      case o: Option[Transaction] => o match {
        case Some(null) => service.prepare(llQuery)
        case Some(t) => service.prepare(t, llQuery)
        case None => service.prepare(null, llQuery)
      }
      case _ =>
        service.prepare(llQuery)
    }
  }

  def asSeqWithTx[E <: Entity[E]](tx: Transaction)(implicit meta: EntityMeta[E]): Seq[E] = _asSeq(Some(tx), Datastore.query)

  def asSeqWithTx[E <: Entity[E]](tx: Transaction, query: Query[E])(implicit meta: EntityMeta[E]): Seq[E] = _asSeq(Some(tx), query)

  def asSeqWithoutTx[E <: Entity[E]](implicit meta: EntityMeta[E]): Seq[E] = _asSeq(Some(null), Datastore.query)

  def asSeqWithoutTx[E <: Entity[E]](query: Query[E])(implicit meta: EntityMeta[E]): Seq[E] = _asSeq(Some(null), query)

  def asSeq[E <: Entity[E]](implicit meta: EntityMeta[E]): Seq[E] = _asSeq(None, Datastore.query)

  def asSeq[E <: Entity[E]](query: Query[E])(implicit meta: EntityMeta[E]): Seq[E] = _asSeq(None, query)

  private def _asSeq[E <: Entity[E]](tx: Option[Transaction], query: Query[E])(implicit meta: EntityMeta[E]): Seq[E] = {
    val q = query
    val options = (q.offset, q.limit) match {
      case (Some(o), Some(l)) => FetchOptions.Builder.withOffset(o).limit(l)
      case (Some(o), _) => FetchOptions.Builder.withOffset(o)
      case (_, Some(o)) => FetchOptions.Builder.withLimit(o)
      case _ => FetchOptions.Builder.withDefaults()
    }
    prepare(tx, q, keysOnly = false).asIterable(options).map {
      e => q.meta.toEntity(e)
    }.toSeq
  }

  def asSingleWithTx[E <: Entity[E]](tx: Transaction)(implicit meta: EntityMeta[E]): E = _asSingle(Some(tx), Datastore.query)

  def asSingleWithTx[E <: Entity[E]](tx: Transaction, query: Query[E])(implicit meta: EntityMeta[E]): E = _asSingle(Some(tx), query)

  def asSingleWithoutTx[E <: Entity[E]](implicit meta: EntityMeta[E]): E = _asSingle(Some(null), Datastore.query)

  def asSingleWithoutTx[E <: Entity[E]](query: Query[E])(implicit meta: EntityMeta[E]): E = _asSingle(Some(null), query)

  def asSingle[E <: Entity[E]](implicit meta: EntityMeta[E]): E = _asSingle(None, Datastore.query)

  def asSingle[E <: Entity[E]](query: Query[E])(implicit meta: EntityMeta[E]): E = _asSingle(None, query)

  private def _asSingle[E <: Entity[E]](tx: Option[Transaction], query: Query[E])(implicit meta: EntityMeta[E]): E = {
    val q = query
    prepare(tx, q, keysOnly = false).asSingleEntity() match {
      case singleEntity: LLEntity => q.meta.toEntity(singleEntity)
      case null => throw new IllegalArgumentException(s"Entity not found for $q.")
    }
  }

  def asSingleOptionWithTx[E <: Entity[E]](tx: Transaction)(implicit meta: EntityMeta[E]): Option[E] = _asSingleOption(Some(tx), Datastore.query)

  def asSingleOptionWithTx[E <: Entity[E]](tx: Transaction, query: Query[E])(implicit meta: EntityMeta[E]): Option[E] = _asSingleOption(Some(tx), query)

  def asSingleOptionWithoutTx[E <: Entity[E]](implicit meta: EntityMeta[E]): Option[E] = _asSingleOption(Some(null), Datastore.query)

  def asSingleOptionWithoutTx[E <: Entity[E]](query: Query[E])(implicit meta: EntityMeta[E]): Option[E] = _asSingleOption(Some(null), query)

  def asSingleOption[E <: Entity[E]](implicit meta: EntityMeta[E]): Option[E] = _asSingleOption(None, Datastore.query)

  def asSingleOption[E <: Entity[E]](query: Query[E])(implicit meta: EntityMeta[E]): Option[E] = _asSingleOption(None, query)

  private def _asSingleOption[E <: Entity[E]](tx: Option[Transaction], query: Query[E])(implicit meta: EntityMeta[E]): Option[E] = {
    val q = query
    prepare(tx, q, keysOnly = false).asSingleEntity() match {
      case s: LLEntity => Some(q.meta.toEntity(s))
      case e => None
    }
  }

  def asKeySeqWithTx[E <: Entity[E]](tx: Transaction)(implicit meta: EntityMeta[E]): Seq[Key[E]] = _asKeySeq(Some(tx), Datastore.query)

  def asKeySeqWithTx[E <: Entity[E]](tx: Transaction, query: Query[E])(implicit meta: EntityMeta[E]): Seq[Key[E]] = _asKeySeq(Some(tx), query)

  def asKeySeqWithoutTx[E <: Entity[E]](implicit meta: EntityMeta[E]): Seq[Key[E]] = _asKeySeq(Some(null), Datastore.query)

  def asKeySeqWithoutTx[E <: Entity[E]](query: Query[E])(implicit meta: EntityMeta[E]): Seq[Key[E]] = _asKeySeq(Some(null), query)

  def asKeySeq[E <: Entity[E]](implicit meta: EntityMeta[E]): Seq[Key[E]] = _asKeySeq(None, Datastore.query)

  def asKeySeq[E <: Entity[E]](query: Query[E])(implicit meta: EntityMeta[E]): Seq[Key[E]] = _asKeySeq(None, query)

  private def _asKeySeq[E <: Entity[E]](tx: Option[Transaction], query: Query[E])(implicit meta: EntityMeta[E]): Seq[Key[E]] = {
    val q = query
    val options = (q.offset, q.limit) match {
      case (Some(o), Some(l)) => FetchOptions.Builder.withOffset(o).limit(l)
      case (Some(o), _) => FetchOptions.Builder.withOffset(o)
      case (_, Some(o)) => FetchOptions.Builder.withLimit(o)
      case _ => FetchOptions.Builder.withDefaults()
    }
    prepare(tx, q, keysOnly = true).asIterable(options).map {
      e => q.meta.createKey(e.getKey)
    }.toSeq
  }
}

sealed private[datastore] trait DatastoreTxMethods {
  self: DatastoreBase =>

  def beginTx: Transaction = service.beginTransaction()

  def beginXgTx: Transaction = service.beginTransaction(TransactionOptions.Builder.withXG(true))

  def beginTx(option: TransactionOptions): Transaction = service.beginTransaction(option)

  def currentTx: Transaction = service.getCurrentTransaction

  def currentTx(tx: Transaction): Transaction = service.getCurrentTransaction(tx)

  def activeTx: Seq[Transaction] = service.getActiveTransactions.toSeq

  def tx[T](block: => T): T = {
    val tx = beginTx
    try {
      val ret = block
      tx.commit()
      ret
    } finally {
      if (tx.isActive) tx.rollback()
    }
  }

  def xgTx[T](block: => T): T = {
    val tx = beginXgTx
    try {
      val ret = block
      tx.commit()
      ret
    } finally {
      if (tx.isActive) tx.rollback()
    }
  }
}

case class DatastoreQuery[E <: Entity[E]](datastore: Datastore, query: Query[E]) {

  implicit val entityMeta = query.meta

  def asSeq = datastore.asSeq(query)

  def asKeySeq = datastore.asKeySeq(query)

  def asSingle = datastore.asSingle(query)

  def asSingleOption = datastore.asSingleOption(query)

  def count = datastore.count(query)
}

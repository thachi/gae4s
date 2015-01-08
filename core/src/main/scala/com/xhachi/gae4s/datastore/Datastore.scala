package com.xhachi.gae4s.datastore

import com.google.appengine.api.datastore._

import scala.collection.JavaConversions._

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
  with DatastoreTxMethods

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
    case Some(k) => getOption(k) match {
      case Some(e) => throw new IllegalStateException("entity was already stored")
      case None => putWithTx(tx, entity)
    }
    case None => putWithTx(tx, entity)
  }

  def create[E <: Entity[E]](entity: E)(implicit meta: EntityMeta[E]): Key[E] = entity.keyOption match {
    case Some(k) => getOption(k) match {
      case Some(e) => throw new IllegalStateException("entity was already stored")
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
    if (getWithTx[E](tx, keys).exists(_._2 == null)) throw new IllegalStateException("entity which is stored already is included")
    putWithTx(tx, entities)
  }

  def create[E <: Entity[E]](entities: Seq[E])(implicit meta: EntityMeta[E]): Seq[Key[E]] = {
    val keys = entities.map(_.key)
    if (get[E](keys).exists(_._2 == null)) throw new IllegalStateException("entity which is stored already is included")
    put(entities)
  }
}

sealed private[datastore] trait DatastoreUpdateMethods {
  self: DatastoreBase with DatastorePutMethods with DatastoreGetOptionMethods =>

  def updateWithoutTx[E <: Entity[E]](entity: E)(implicit meta: EntityMeta[E]): Key[E] = updateWithTx(null, entity)

  def updateWithTx[E <: Entity[E]](tx: Transaction, entity: E)(implicit meta: EntityMeta[E]): Key[E] = getOptionWithTx(tx, entity.key) match {
    case Some(e) if isSameVersion(entity, e) => putWithTx(tx, entity)
    case None => throw new IllegalStateException()
  }

  def update[E <: Entity[E]](entity: E)(implicit meta: EntityMeta[E]): Key[E] = if(meta.versionEnabled) {
    getOption(entity.key) match {
      case Some(e) if isSameVersion(entity, e) => put(entity)
      case Some(e) => throw new IllegalStateException("invalid version property. %s store:%d, stored:%d".format(entity.key, meta.version(entity).get, meta.version(e).get))
      case None => throw new IllegalStateException("entity is not exists in datastore.")
    }
  } else {
    put(entity)
  }

  def isSameVersion[E <: Entity[E]](entity1: E, entity2: E)(implicit meta: EntityMeta[E]) =
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
    if (invalids.nonEmpty) throw new IllegalStateException("invalid version property.\n" + invalids.mkString("\n"))
    putWithTx(tx, entities)
  }

  def update[E <: Entity[E]](entities: Seq[E])(implicit meta: EntityMeta[E]): Seq[Key[E]] = {
    val got = get[E](entities.map(_.key)).values.toSeq
    val invalids = getInvalidVersion(entities.sortBy(_.key), got.sortBy(_.key))
    if (invalids.nonEmpty) throw new IllegalStateException("invalid version property. " + invalids)
    put(entities)
  }

  def getInvalidVersion[E <: Entity[E]](entity1: Seq[E], entity2: Seq[E])(implicit meta: EntityMeta[E]): Seq[String] = {
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


  //todo Mいらない
  def query[E <: Entity[E]](implicit meta: EntityMeta[E]) =
    Query[E](this, meta, None)

  def query[E <: Entity[E]](ancestor: Key[_])(implicit meta: EntityMeta[E]) =
    Query[E](this, meta, None, Some(ancestor))

  def queryWithTx[E <: Entity[E]](tx: Transaction)(implicit meta: EntityMeta[E]) =
    Query[E](this, meta, Some(tx))

  def queryWithTx[E <: Entity[E]](tx: Transaction, ancestor: Key[_])(implicit meta: EntityMeta[E]) =
    Query[E](this, meta, Some(tx), Some(ancestor))

  def queryWithoutTx[E <: Entity[E]](implicit meta: EntityMeta[E]) =
    Query[E](this, meta, null)

  def queryWithoutTx[E <: Entity[E]](ancestor: Key[_])(implicit meta: EntityMeta[E]) =
    Query[E](this, meta, null, Some(ancestor))

  def count[E <: Entity[E]](query: Query[E]): Int = {
    val llQuery = query.toLLQuery(keysOnly = false)
    service.prepare(llQuery).countEntities(FetchOptions.Builder.withLimit(Int.MaxValue))
  }

  private def prepare[E <: Entity[E]](query: Query[E], keysOnly: Boolean): PreparedQuery = {
    val llQuery = query.toLLQuery(keysOnly = keysOnly)
    query.tx match {
      case o: Option[Transaction] => o match {
        case Some(tx) => service.prepare(tx, llQuery)
        case None => service.prepare(null, llQuery)
      }
      case _ =>
        service.prepare(llQuery)
    }
  }

  def asSeq[E <: Entity[E]](query: Query[E]): Seq[E] = {
    val options = (query.offset, query.limit) match {
      case (Some(o), Some(l)) => FetchOptions.Builder.withOffset(o).limit(l)
      case (Some(o), _) => FetchOptions.Builder.withOffset(o)
      case (_, Some(o)) => FetchOptions.Builder.withLimit(o)
      case _ => FetchOptions.Builder.withDefaults()
    }
    prepare(query, keysOnly = false).asIterable(options).map {
      e => query.meta.toEntity(e)
    }.toSeq
  }

  def asSingle[E <: Entity[E]](query: Query[E]): E = {
    query.meta.toEntity(prepare(query, keysOnly = false).asSingleEntity())
  }

  def asSingleOption[E <: Entity[E]](query: Query[E]): Option[E] = {
    query.meta.toEntity(prepare(query, keysOnly = false).asSingleEntity()) match {
      case null => None
      case e => Some(e)
    }
  }


  def asKeySeq[E <: Entity[E]](query: Query[E]): Seq[Key[E]] = {
    val options = (query.offset, query.limit) match {
      case (Some(o), Some(l)) => FetchOptions.Builder.withOffset(o).limit(l)
      case (Some(o), _) => FetchOptions.Builder.withOffset(o)
      case (_, Some(o)) => FetchOptions.Builder.withLimit(o)
      case _ => FetchOptions.Builder.withDefaults()
    }
    prepare(query, keysOnly = true).asIterable(options).map {
      e => query.meta.createKey(e.getKey)
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

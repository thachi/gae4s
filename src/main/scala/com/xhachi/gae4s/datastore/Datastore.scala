package com.xhachi.gae4s.datastore

import scala.collection.JavaConversions._
import com.google.appengine.api.datastore._


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


object Datastore extends Datastore(DatastoreServiceFactory.getDatastoreService) {
  def apply(service: DatastoreService) = new Datastore(service)
}


sealed trait DatastoreBase {

  private[datastore] def service: DatastoreService
}

sealed trait DatastoreGetMethods extends DatastoreBase {

  def getWithoutTx[E <: Entity[E]](key: Key[E])(implicit meta: EntityMeta[E]): E = getWithTx(null, key)

  def getWithTx[E <: Entity[E]](tx: Transaction, key: Key[E])(implicit meta: EntityMeta[E]): E = meta.toEntity(service.get(tx, key.key))

  def get[E <: Entity[E]](key: Key[E])(implicit meta: EntityMeta[E]): E = meta.toEntity(service.get(key.key))
}

sealed trait DatastoreGetOptionMethods extends DatastoreBase with DatastoreGetMethods {

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

sealed trait DatastoreGetListMethods extends DatastoreBase {

  def getWithoutTx[E <: Entity[E]](keys: Seq[Key[E]])(implicit meta: EntityMeta[E]): Map[Key[E], E] = getWithTx(null, keys)

  def getWithTx[E <: Entity[E]](tx: Transaction, keys: Seq[Key[E]])(implicit meta: EntityMeta[E]): Map[Key[E], E] = {
    val entities = service.get(tx, keys.map(_.key)).map {
      case (k, v) => Key[E](k) -> meta.toEntity(v)
    }
    entities.asInstanceOf[Map[Key[E], E]]
  }

  def get[E <: Entity[E]](keys: Seq[Key[E]])(implicit meta: EntityMeta[E]): Map[Key[E], E] = {
    val entities = service.get(keys.map(_.key)).map {
      case (k, v) => Key[E](k) -> meta.toEntity(v)
    }
    entities.asInstanceOf[Map[Key[E], E]]
  }
}

sealed trait DatastoreDeleteMethods extends DatastoreBase {

  def deleteWithoutTx[E <: Entity[E]](key: Key[E])(implicit meta: EntityMeta[E]): Unit = deleteWithTx(null, key)

  def deleteWithTx[E <: Entity[E]](tx: Transaction, key: Key[E])(implicit meta: EntityMeta[E]): Unit = service.delete(tx, key.key)

  def delete[E <: Entity[E]](key: Key[E])(implicit meta: EntityMeta[E]): Unit = service.delete(key.key)
}

sealed trait DatastoreDeleteListMethods extends DatastoreBase {

  def deleteWithoutTx[E <: Entity[E]](keys: Seq[Key[E]])(implicit meta: EntityMeta[E]): Unit = deleteWithTx(null, keys)

  def deleteWithTx[E <: Entity[E]](tx: Transaction, keys: Seq[Key[E]])(implicit meta: EntityMeta[E]): Unit = service.delete(tx, keys.map(_.key))

  def delete[E <: Entity[E]](keys: Seq[Key[E]])(implicit meta: EntityMeta[E]): Unit = service.delete(keys.map(_.key))
}

sealed trait DatastorePutMethods extends DatastoreBase {

  def putWithoutTx[E <: Entity[E]](entity: E)(implicit meta: EntityMeta[E]): Key[E] = putWithTx(null, entity)

  def putWithTx[E <: Entity[E]](tx: Transaction, entity: E)(implicit meta: EntityMeta[E]): Key[E] = {
    val e = meta.toLLEntity(entity)
    Key(service.put(tx, e))
  }

  def put[E <: Entity[E]](entity: E)(implicit meta: EntityMeta[E]): Key[E] = {
    val e = meta.toLLEntity(entity)
    Key(service.put(e))
  }
}

sealed trait DatastorePutListMethods extends DatastoreBase {

  def putWithoutTx[E <: Entity[E]](entities: Seq[E])(implicit meta: EntityMeta[E]): Seq[Key[E]] = putWithTx(null, entities)

  def putWithTx[E <: Entity[E]](tx: Transaction, entities: Seq[E])(implicit meta: EntityMeta[E]): Seq[Key[E]] = {
    val e = entities.map(meta.toLLEntity)
    service.put(tx, e).map(Key[E]).toSeq
  }

  def put[E <: Entity[E]](entities: Seq[E])(implicit meta: EntityMeta[E]): Seq[Key[E]] = {
    val e = entities.map(meta.toLLEntity)
    service.put(e).map(Key[E]).toSeq
  }
}

sealed trait DatastoreCreateMethods extends DatastoreBase with DatastorePutMethods with DatastoreGetOptionMethods {

  def createWithoutTx[E <: Entity[E]](tx: Transaction, entity: E)(implicit meta: EntityMeta[E]): Key[E] = createWithTx(null, entity)

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

sealed trait DatastoreCreateListMethods extends DatastoreBase with DatastorePutListMethods with DatastoreGetListMethods {

  def createWithoutTx[E <: Entity[E]](tx: Transaction, entities: Seq[E])(implicit meta: EntityMeta[E]): Seq[Key[E]] = createWithTx(null, entities)

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

sealed trait DatastoreUpdateMethods extends DatastoreBase with DatastorePutMethods with DatastoreGetOptionMethods {

  def updateWithoutTx[E <: Entity[E]](entity: E)(implicit meta: EntityMeta[E]): Key[E] = updateWithTx(null, entity)

  def updateWithTx[E <: Entity[E]](tx: Transaction, entity: E)(implicit meta: EntityMeta[E]): Key[E] = getOptionWithTx(tx, entity.key) match {
    case Some(e) => putWithTx(tx, entity)
    case None => throw new IllegalStateException()
  }

  def update[E <: Entity[E]](entity: E)(implicit meta: EntityMeta[E]): Key[E] = getOption(entity.key) match {
    case Some(e) if isSameVersion(entity, e) => put(entity)
    case Some(_) => throw new IllegalStateException("invalid version property.")
    case None => throw new IllegalStateException("entity is not exists in datastore.")
  }

  def isSameVersion[E <: Entity[E]](entity1: E, entity2: E) = (entity1, entity2) match {
    case (e1: Version, e2: Version) => e1.version == e2.version
    case _ => true
  }
}

sealed trait DatastoreUpdateListMethods extends DatastoreBase with DatastorePutListMethods with DatastoreGetListMethods {

  def updateWithoutTx[E <: Entity[E]](entities: Seq[E])(implicit meta: EntityMeta[E]): Seq[Key[E]] = updateWithTx(null, entities)

  def updateWithTx[E <: Entity[E]](tx: Transaction, entities: Seq[E])(implicit meta: EntityMeta[E]): Seq[Key[E]] = {
    val got = get[E](entities.map(_.key)).values.toSeq
    if (!isSameVersion(entities, got)) throw new IllegalStateException("invalid version property.")
    putWithTx(tx, entities)
  }

  def update[E <: Entity[E]](entities: Seq[E])(implicit meta: EntityMeta[E]): Seq[Key[E]] = {
    val got = get[E](entities.map(_.key)).values.toSeq
    if (!isSameVersion(entities, got)) throw new IllegalStateException("invalid version property.")
    put(entities)
  }

  def isSameVersion[E](entity1: Seq[E], entity2: Seq[E]): Boolean = {
    assert(entity1.size == entity2.size)
    entity1.zip(entity2).map {
      case (e1: Version, e2: Version) => e1.version == e2.version
      case _ => true
    }.exists(_ == false)
  }
}

sealed trait DatastoreCreateKeyMethods extends DatastoreBase {

  def createKey[E <: Entity[E], M <: EntityMeta[E]](name: String)(implicit meta: M): Key[E] = Key[E](name)

  def createKey[E <: Entity[E], M <: EntityMeta[E]](parent: Key[_], name: String)(implicit meta: M): Key[E] = Key[E](parent, name)

  def createKey[E <: Entity[E], M <: EntityMeta[E]](id: Long)(implicit meta: M): Key[E] = Key[E](id)

  def createKey[E <: Entity[E], M <: EntityMeta[E]](parent: Key[_], id: Long)(implicit meta: M): Key[E] = Key[E](parent, id)

  def allocateKey[E <: Entity[E], M <: EntityMeta[E]]()(implicit meta: M): Key[E] = {
    Key[E](service.allocateIds(meta.kind, 1).getStart.getId)
  }

  def allocateKeys[E <: Entity[E], M <: EntityMeta[E]](count: Long)(implicit meta: M): Seq[Key[E]] = {
    service.allocateIds(meta.kind, count).map(Key[E]).toSeq
  }

  def allocateKey[E <: Entity[E], M <: EntityMeta[E]](parent: Key[_])(implicit meta: M): Key[E] = {
    Key[E](service.allocateIds(parent.key, meta.kind, 1).getStart)
  }

  def allocateKeys[E <: Entity[E], M <: EntityMeta[E]](parent: Key[_], count: Long)(implicit meta: M): Seq[Key[E]] = {
    service.allocateIds(parent.key, meta.kind, count).map(Key[E]).toSeq
  }
}

sealed trait DatastoreQueryMethods extends DatastoreBase {

  def query[E <: Entity[E], M <: EntityMeta[E]](implicit meta: M) =
    Query[E, M](this, meta, None)

  def query[E <: Entity[E], M <: EntityMeta[E]](ancestor: Key[_])(implicit meta: M) =
    Query[E, M](this, meta, None, Some(ancestor))

  def queryWithTx[E <: Entity[E], M <: EntityMeta[E]](tx: Transaction)(implicit meta: M) =
    Query[E, M](this, meta, Some(tx))

  def queryWithTx[E <: Entity[E], M <: EntityMeta[E]](tx: Transaction, ancestor: Key[_])(implicit meta: M) =
    Query[E, M](this, meta, Some(tx), Some(ancestor))

  def queryWithoutTx[E <: Entity[E], M <: EntityMeta[E]](implicit meta: M) =
    Query[E, M](this, meta, null)

  def queryWithoutTx[E <: Entity[E], M <: EntityMeta[E]](ancestor: Key[_])(implicit meta: M) =
    Query[E, M](this, meta, null, Some(ancestor))

  def count[E <: Entity[E], M <: EntityMeta[E]](query: Query[E, M]): Int = {
    val llQuery = query.toLLQuery(keysOnly = false)
    service.prepare(llQuery).countEntities(FetchOptions.Builder.withLimit(Int.MaxValue))
  }

  private def prepare[E <: Entity[E], M <: EntityMeta[E]](query: Query[E, M], keysOnly: Boolean): PreparedQuery = {
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

  def asSeq[E <: Entity[E], M <: EntityMeta[E]](query: Query[E, M]): Seq[E] = {
    prepare(query, keysOnly = false).asIterable.map {
      e => query.meta.toEntity(e)
    }.toSeq
  }

  def asSingle[E <: Entity[E], M <: EntityMeta[E]](query: Query[E, M]): E = {
    query.meta.toEntity(prepare(query, keysOnly = false).asSingleEntity())
  }

  def asKeySeq[E <: Entity[E], M <: EntityMeta[E]](query: Query[E, M]): Seq[Key[E]] = {
    prepare(query, keysOnly = true).asIterable.map {
      e => Key[E](e.getKey)
    }.toSeq
  }
}


sealed trait DatastoreTxMethods extends DatastoreBase {

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

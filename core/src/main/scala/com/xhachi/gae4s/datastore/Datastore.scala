package com.xhachi.gae4s.datastore

import java.util.ConcurrentModificationException

import com.google.appengine.api.datastore.Query.{CompositeFilter => LLCompositeFilter, Filter => LLFilter, FilterPredicate => LLFilterPredicate}
import com.google.appengine.api.datastore.{Query => LLQuery, _}

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

  def getWithoutTx(key: Key): Entity = getWithTx(null, key)

  def getWithTx(tx: Transaction, key: Key): Entity = Entity(service.get(tx, key.key))

  def get(key: Key): Entity = Entity(service.get(key.key))
}

sealed private[datastore] trait DatastoreGetOptionMethods {
  self: DatastoreBase with DatastoreGetMethods =>

  def getOptionWithoutTx(key: Key): Option[Entity] = {
    getOptionWithTx(null, key)
  }

  def getOptionWithTx(tx: Transaction, key: Key): Option[Entity] = try {
    Some(getWithTx(tx, key))
  } catch {
    case e: EntityNotFoundException => None
  }

  def getOption(key: Key): Option[Entity] = try {
    Some(get(key))
  } catch {
    case e: EntityNotFoundException => None
  }
}

sealed private[datastore] trait DatastoreGetListMethods {
  self: DatastoreBase =>

  def getWithoutTx(keys: Seq[Key]): Map[Key, Entity] = getWithTx(null, keys)

  def getWithTx(tx: Transaction, keys: Seq[Key]): Map[Key, Entity] = {
    val entities = service.get(tx, keys.map(_.key)).map {
      case (k, v) => Key(k) -> Entity(v)
    }
    entities.toMap
  }

  def get(keys: Seq[Key]): Map[Key, Entity] = {
    val got = service.get(keys.map(_.key))
    val entities = got.map {
      case (k, v) => Key(k) -> Entity(v)
    }
    entities.toMap
  }
}

sealed private[datastore] trait DatastoreDeleteMethods {
  self: DatastoreBase =>

  def deleteWithoutTx(key: Key): Unit = deleteWithTx(null, key)

  def deleteWithTx(tx: Transaction, key: Key): Unit = service.delete(tx, key.key)

  def delete(key: Key): Unit = service.delete(key.key)
}

sealed private[datastore] trait DatastoreDeleteListMethods {
  self: DatastoreBase =>

  def deleteWithoutTx(keys: Seq[Key]): Unit = deleteWithTx(null, keys)

  def deleteWithTx(tx: Transaction, keys: Seq[Key]): Unit = service.delete(tx, keys.map(_.key))

  def delete(keys: Seq[Key]): Unit = service.delete(keys.map(_.key))
}

sealed private[datastore] trait DatastorePutMethods extends DatastoreBase {

  def putWithoutTx(entity: Entity): Key = putWithTx(null, entity)

  def putWithTx(tx: Transaction, entity: Entity): Key = {
    val e = entity.entity
    Key(service.put(tx, e))
  }

  def put(entity: Entity): Key = {
    val e = entity.entity
    Key(service.put(e))
  }
}

sealed private[datastore] trait DatastorePutListMethods {
  self: DatastoreBase =>

  def putWithoutTx(entities: Seq[Entity]): Seq[Key] = putWithTx(null, entities)

  def putWithTx(tx: Transaction, entities: Seq[Entity]): Seq[Key] = {
    val e = entities.map(_.entity)
    service.put(tx, e).map(k => Key(k)).toSeq
  }

  def put(entities: Seq[Entity]): Seq[Key] = {
    val e = entities.map(_.entity)
    service.put(e).map(k => Key(k)).toSeq
  }
}

sealed private[datastore] trait DatastoreCreateMethods {
  self: DatastoreBase with DatastorePutMethods with DatastoreGetOptionMethods =>


  def createWithoutTx(entity: Entity): Key = createWithTx(null, entity)

  def createWithTx(tx: Transaction, entity: Entity): Key = getOptionWithTx(tx, entity.key) match {
    case Some(e) => throw new ConcurrentModificationException("entity was already stored")
    case None => putWithTx(tx, entity)
  }

  def create(entity: Entity): Key = getOption(entity.key) match {
    case Some(e) => throw new ConcurrentModificationException("entity was already stored")
    case None => put(entity)
  }
}

sealed private[datastore] trait DatastoreCreateListMethods {
  self: DatastoreBase with DatastorePutListMethods with DatastoreGetListMethods =>

  def createWithoutTx(entities: Seq[Entity]): Seq[Key] = createWithTx(null, entities)

  def createWithTx(tx: Transaction, entities: Seq[Entity]): Seq[Key] = {
    val keys = entities.map(_.key)
    if (getWithTx(tx, keys).exists(_._2 == null)) throw new ConcurrentModificationException("entity which is stored already is included")
    putWithTx(tx, entities)
  }

  def create(entities: Seq[Entity]): Seq[Key] = {
    val keys = entities.map(_.key)
    if (get(keys).exists(_._2 == null)) throw new ConcurrentModificationException("entity which is stored already is included")
    put(entities)
  }
}

sealed private[datastore] trait DatastoreUpdateMethods {
  self: DatastoreBase with DatastorePutMethods with DatastoreGetOptionMethods =>

  def updateWithoutTx(entity: Entity): Key = updateWithTx(null, entity)

  def updateWithTx(tx: Transaction, entity: Entity): Key = (entity.versionProperty, getOptionWithTx(tx, entity.key).flatMap(_.versionProperty)) match {
    case (Some(v1), Some(v2)) if v1.value == v2.value => putWithTx(tx, entity)
    case (Some(v1), Some(v2)) => throw new ConcurrentModificationException("invalid version property. %s store:%d, stored:%d".format(entity.key, v1.value, v2.value))
    case (Some(v1), None) => throw new IllegalStateException("entity is not exists in datastore.")
    case (None, _) => putWithTx(tx, entity)
  }

  def update(entity: Entity): Key = (entity.versionProperty, getOption(entity.key).flatMap(_.versionProperty)) match {
    case (Some(v1), Some(v2)) if v1.value == v2.value => put(entity)
    case (Some(v1), Some(v2)) => throw new ConcurrentModificationException("invalid version property. %s store:%d, stored:%d".format(entity.key, v1.value, v2.value))
    case (Some(v1), None) => throw new IllegalStateException("entity is not exists in datastore.")
    case (None, _) => put(entity)
  }
}

sealed private[datastore] trait DatastoreUpdateListMethods {
  self: DatastoreBase with DatastorePutListMethods with DatastoreGetListMethods =>

  def updateWithoutTx(entities: Seq[Entity]): Seq[Key] = updateWithTx(null, entities)

  def updateWithTx(tx: Transaction, entities: Seq[Entity]): Seq[Key] = {
    val got = get(entities.map(_.key)).values.toSeq
    val invalids = getInvalidVersion(entities, got)
    if (invalids.nonEmpty) throw new ConcurrentModificationException("invalid version property.\n" + invalids.mkString("\n"))
    putWithTx(tx, entities)
  }

  def update(entities: Seq[Entity]): Seq[Key] = {
    val got = get(entities.map(_.key)).values.toSeq
    val invalids = getInvalidVersion(entities.sortBy(_.key), got.sortBy(_.key))
    if (invalids.nonEmpty) throw new ConcurrentModificationException("invalid version property. " + invalids)
    put(entities)
  }

  private def getInvalidVersion(entity1: Seq[Entity], entity2: Seq[Entity]): Seq[String] = {
    assert(entity1.size == entity2.size)
    entity1.zip(entity2)
      .filterNot { case (e1, e2) => e1.isSameVersion(e2) }
      .map { case (e1, e2) => "%s store:%d, stored:%d".format(e1.key, e1.version.get, e2.version.get) }
  }
}

sealed private[datastore] trait DatastoreCreateKeyMethods {
  self: DatastoreBase =>

  def createKey(kind: String, name: String): Key = Key(KeyFactory.createKey(kind, name))

  def createKey(kind: String, parent: Key, name: String): Key = Key(KeyFactory.createKey(parent.key, kind, name))

  def createKey(kind: String, id: Long): Key = Key(KeyFactory.createKey(kind, id))

  def createKey(kind: String, parent: Key, id: Long): Key = Key(KeyFactory.createKey(parent.key, kind, id))

  def allocateKey(kind: String): Key = Key(service.allocateIds(kind, 1).getStart)


  def allocateKeys(kind: String, count: Long): Seq[Key] = service.allocateIds(kind, count).map(Key(_)).toSeq

  def allocateKey(kind: String, parent: Key): Key = allocateKeys(kind, 1L).head

  def allocateKeys(kind: String, parent: Key, count: Long): Seq[Key] = service.allocateIds(parent.key, kind, count).map(Key(_)).toSeq
}

sealed private[datastore] trait DatastoreQueryMethods {
  self: DatastoreBase =>
  //
  //  def query(kind: String): Query = Query(kind)
  //
  //  def query(kind: String, ancestor: Key): Query = Query(kind).ancestor(ancestor)
  //
  //  def countWithTx(tx: Transaction): Int = _count(Some(tx), Query)
  //
  //  def countWithTx(tx: Transaction, query: Query): Int = _count(Some(tx), query)
  //
  //  def countWithoutTx: Int = _count(Some(null), Query)
  //
  //  def countWithoutTx(query: Query): Int = _count(Some(null), query)
  //
  def count(kind: String): Int = _count(None, Query(kind))

  //
  //  def count(query: Query): Int = _count(None, query)
  //
  private def _count(tx: Option[Transaction], query: Query): Int = {
    val llQuery = _toLLQuery(query, keysOnly = false)
    service.prepare(llQuery).countEntities(FetchOptions.Builder.withLimit(Int.MaxValue))
  }

  private def prepare(tx: Option[Transaction], query: Query, keysOnly: Boolean): PreparedQuery = {
    val llQuery = _toLLQuery(query, keysOnly = keysOnly)
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

  def asSeqWithTx(tx: Transaction, kind: String): Seq[Entity] = _asSeq(Some(tx), Query(kind))

  def asSeqWithTx(tx: Transaction, query: Query): Seq[Entity] = _asSeq(Some(tx), query)

  def asSeqWithoutTx(kind: String): Seq[Entity] = _asSeq(Some(null), Query(kind))

  def asSeqWithoutTx(query: Query): Seq[Entity] = _asSeq(Some(null), query)

  def asSeq(kind: String): Seq[Entity] = _asSeq(None, Query(kind))

  def asSeq(query: Query): Seq[Entity] = _asSeq(None, query)

  private def _asSeq(tx: Option[Transaction], query: Query): Seq[Entity] = {
    val q = query
    val options = (q.offset, q.limit) match {
      case (Some(o), Some(l)) => FetchOptions.Builder.withOffset(o).limit(l)
      case (Some(o), _) => FetchOptions.Builder.withOffset(o)
      case (_, Some(o)) => FetchOptions.Builder.withLimit(o)
      case _ => FetchOptions.Builder.withDefaults()
    }
    prepare(tx, q, keysOnly = false).asIterable(options).map {
      e => Entity(e)
    }.toSeq
  }

  //
  //  def asSingleWithTx(tx: Transaction): Entity = _asSingle(Some(tx), Query)
  //
  //  def asSingleWithTx(tx: Transaction, query: Query): Entity = _asSingle(Some(tx), query)
  //
  //  def asSingleWithoutTx: Entity = _asSingle(Some(null), Query)
  //
  //  def asSingleWithoutTx(query: Query): Entity = _asSingle(Some(null), query)
  //
  //  def asSingle: Entity = _asSingle(None, Query)
  //
  //  def asSingle(query: Query): Entity = _asSingle(None, query)
  //
  //  private def _asSingle(tx: Option[Transaction], query: Query): Entity = {
  //    val q = query
  //    prepare(tx, q, keysOnly = false).asSingleEntity() match {
  //      case singleEntity: LLEntity => q.meta.toEntity(singleEntity)
  //      case null => throw new IllegalArgumentException(s"Entity not found for $q.")
  //    }
  //  }
  //
  //  def asSingleOptionWithTx(tx: Transaction): Option = _asSingleOption(Some(tx), Query)
  //
  //  def asSingleOptionWithTx(tx: Transaction, query: Query): Option = _asSingleOption(Some(tx), query)
  //
  //  def asSingleOptionWithoutTx: Option = _asSingleOption(Some(null), Query)
  //
  //  def asSingleOptionWithoutTx(query: Query): Option = _asSingleOption(Some(null), query)
  //
  //  def asSingleOption: Option = _asSingleOption(None, Query)
  //
  //  def asSingleOption(query: Query): Option = _asSingleOption(None, query)
  //
  //  private def _asSingleOption(tx: Option[Transaction], query: Query): Option = {
  //    val q = query
  //    prepare(tx, q, keysOnly = false).asSingleEntity() match {
  //      case s: LLEntity => Some(q.meta.toEntity(s))
  //      case e => None
  //    }
  //  }
  //
  //  def asKeySeqWithTx(tx: Transaction): Seq[Key] = _asKeySeq(Some(tx), Query)
  //
  //  def asKeySeqWithTx(tx: Transaction, query: Query): Seq[Key] = _asKeySeq(Some(tx), query)
  //
  //  def asKeySeqWithoutTx: Seq[Key] = _asKeySeq(Some(null), Query)
  //
  //  def asKeySeqWithoutTx(query: Query): Seq[Key] = _asKeySeq(Some(null), query)
  //
  //  def asKeySeq: Seq[Key] = _asKeySeq(None, Query)
  //
  //  def asKeySeq(query: Query): Seq[Key] = _asKeySeq(None, query)
  //
  //  private def _asKeySeq(tx: Option[Transaction], query: Query): Seq[Key] = {
  //    val q = query
  //    val options = (q.offset, q.limit) match {
  //      case (Some(o), Some(l)) => FetchOptions.Builder.withOffset(o).limit(l)
  //      case (Some(o), _) => FetchOptions.Builder.withOffset(o)
  //      case (_, Some(o)) => FetchOptions.Builder.withLimit(o)
  //      case _ => FetchOptions.Builder.withDefaults()
  //    }
  //    prepare(tx, q, keysOnly = true).asIterable(options).map {
  //      e => q.meta.createKey(e.getKey)
  //    }.toSeq
  //  }
  //
  //
  private[datastore] def _toLLQuery(query: Query, keysOnly: Boolean): LLQuery = {
    import com.google.appengine.api.datastore.Query.SortDirection._

    val q = new LLQuery(query.kind)
    if (keysOnly) q.setKeysOnly() else q.clearKeysOnly()
    query.ancestorOption.foreach(a => q.setAncestor(a.key))
    query.filterOption.foreach(f => q.setFilter(_toLLFilter(f)))

    query.sorts.foreach(s =>
      q.addSort(
        s.name,
        s.direction match {
          case Sort.Direction.Ascending => ASCENDING
          case Sort.Direction.Descending => DESCENDING
        }
      )
    )
    q
  }

  private[datastore] def _toLLFilter(filter: Filter): LLFilter = {
    import com.google.appengine.api.datastore.Query.CompositeFilterOperator._
    import com.google.appengine.api.datastore.Query.FilterOperator._
    import com.xhachi.gae4s.datastore.Filter._

    filter match {
      case CompositeFilterPredicate(operator, filters) =>
        new LLCompositeFilter(operator match {
          case And => AND
          case Or => OR
        }, filters.map(f => _toLLFilter(f)))
      case FilterPredicate(name, In, values) =>
        new LLFilterPredicate(name, IN, values)
      case FilterPredicate(name, operator, value :: Nil) =>
        val o = operator match {
          case Equal => EQUAL
          case NotEqual => NOT_EQUAL
          case LessThan => LESS_THAN
          case LessThanOrEqual => LESS_THAN_OR_EQUAL
          case GreaterThan => GREATER_THAN
          case GreaterThanOrEqual => GREATER_THAN_OR_EQUAL
          case In => throw new IllegalArgumentException("unexpected filter")
        }
        new LLFilterPredicate(name, o, value)
      case FilterPredicate(_, _, _) => throw new IllegalArgumentException("unexpected filter")
    }
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

//case class DatastoreQuery(datastore: Datastore, query: Query) {
//
//
//  def ancestor(ancestor: Key): DatastoreQuery = copy(query = query.ancestor(ancestor))
//
//  def ancestor(ancestor: Option[Key]): DatastoreQuery = copy(query = query.ancestor(ancestor))
//
//  def filterByMeta(filter: EntityMeta => Filter): DatastoreQuery = copy(query = query.filterByMeta(filter))
//
//  def filter(filter: Filter): DatastoreQuery = copy(query = query.filter(filter))
//
//  //  def filter(filter: Entity => Boolean): DatastoreQuery = macro EntityMacro.filter
//
//  def sortByMeta(sort: EntityMeta => Sort): DatastoreQuery = copy(query = query.sortByMeta(sort))
//
//  def sort(sorts: Sort*): DatastoreQuery = copy(query = query.sort(sorts: _*))
//
//  //  def sort(sort: Entity => Any): DatastoreQuery = macro EntityMacro.sort
//
//  //  def sortDesc(sort: Entity => Any): DatastoreQuery = macro EntityMacro.sortDesc
//
//  def offset(o: Int): DatastoreQuery = copy(query = query.offset(o))
//
//  def offset(o: Option[Int]): DatastoreQuery = copy(query = query.offset(o))
//
//  def limit(l: Int): DatastoreQuery = copy(query = query.limit(l))
//
//  def limit(l: Option[Int]): DatastoreQuery = copy(query = query.limit(l))
//
//  def asSeq = datastore.asSeq(query)
//
//  def asKeySeq = datastore.asKeySeq(query)
//
//  def asSingle = datastore.asSingle(query)
//
//  def asSingleOption = datastore.asSingleOption(query)
//
//  def count = datastore.count(query)
//}

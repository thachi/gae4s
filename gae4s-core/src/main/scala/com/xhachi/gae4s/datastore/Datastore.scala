package com.xhachi.gae4s.datastore

import java.util.ConcurrentModificationException

import com.google.appengine.api.datastore.Query.{FilterOperator, CompositeFilter => LLCompositeFilter, Filter => LLFilter, FilterPredicate => LLFilterPredicate}
import com.google.appengine.api.datastore.{Query => LLQuery, _}

import scala.collection.JavaConverters._

/**
  * Class to access Datastore service.
  *
  * @author Takashi Hachinohe
  * @param service the DatastoreService instance
  */
class Datastore private[datastore](private[datastore] val service: DatastoreService)
  extends DatastoreBase
    with DatastoreGetOps
    with DatastoreGetKeyOps
    with DatastoreGetOptionOps
    with DatastoreGetProjectionListOps
    with DatastoreGetProjectionOps
    with DatastoreGetListOps
    with DatastorePutOps
    with DatastorePutListOps
    with DatastoreCreateOps
    with DatastoreCreateListOps
    with DatastoreUpdateOps
    with DatastoreUpdateListOps
    with DatastoreDeleteOps
    with DatastoreDeleteListOps
    with DatastoreQueryOps
    with DatastoreCreateKeyOps
    with DatastoreTxOps {
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

sealed private[datastore] trait DatastoreGetKeyOps {
  self: DatastoreBase =>

  def getKeysWithoutTx: Seq[Key] = getKeysWithTx(null)

  def getKeysWithTx(tx: Transaction): Seq[Key] = service.prepare(tx, new LLQuery().setKeysOnly()).asIterable().asScala.map(e => Key(e.getKey)).toSeq

  def getKeys: Seq[Key] = service.prepare(new LLQuery().setKeysOnly()).asIterable().asScala.map(e => Key(e.getKey)).toSeq
}

sealed private[datastore] trait DatastoreGetOps {
  self: DatastoreBase =>

  def getWithoutTx(key: Key): Entity = getWithTx(null, key)

  def getWithTx(tx: Transaction, key: Key): Entity = Entity(service.get(tx, key.key))

  def get(key: Key): Entity = Entity(service.get(key.key))
}

sealed private[datastore] trait DatastoreGetProjectionOps {
  self: DatastoreBase =>

  def getProjectionWithoutTx(key: Key, properties: Map[String, Class[_]]): Entity = getProjectionWithTx(null, key, properties)

  def getProjectionWithTx(tx: Transaction, key: Key, properties: Map[String, Class[_]]): Entity = {
    val q = new LLQuery(key.kind)
      .setFilter(new LLFilterPredicate("__key__", FilterOperator.EQUAL, key.key))
    properties.foreach { case (name, clazz) => q.addProjection(new PropertyProjection(name, clazz)) }

    import scala.collection.JavaConverters._
    val entities = service.prepare(tx, q).asList(FetchOptions.Builder.withLimit(1)).asScala.toList
    entities.headOption.map(Entity(_)).getOrElse(throw new IllegalArgumentException(s"$key is not found."))
  }

  def getProjection(key: Key, properties: Map[String, Class[_]]): Entity = {
    val q = new LLQuery(key.kind)
      .setFilter(new LLFilterPredicate("__key__", FilterOperator.EQUAL, key.key))
    properties.foreach { case (name, clazz) => q.addProjection(new PropertyProjection(name, clazz)) }

    import scala.collection.JavaConverters._
    val entities = service.prepare(q).asList(FetchOptions.Builder.withLimit(1)).asScala.toList
    entities.headOption.map(Entity(_)).getOrElse(throw new IllegalArgumentException(s"$key is not found."))
  }
}

sealed private[datastore] trait DatastoreGetOptionOps {
  self: DatastoreBase with DatastoreGetOps =>

  def getOptionWithoutTx(key: Key): Option[Entity] = {
    getOptionWithTx(null, key)
  }

  //TODO: 存在しない場合はNoneが返り、データアクセスに失敗した場合は例外が発生する。これで良いか検討する。
  def getOptionWithTx(tx: Transaction, key: Key): Option[Entity] = try {
    Some(getWithTx(tx, key))
  } catch {
    case _: EntityNotFoundException => None
  }

  def getOption(key: Key): Option[Entity] = try {
    Some(get(key))
  } catch {
    case _: EntityNotFoundException => None
  }
}

sealed private[datastore] trait DatastoreGetListOps {
  self: DatastoreBase =>

  def getWithoutTx(keys: Seq[Key]): Map[Key, Entity] = getWithTx(null, keys)

  def getWithTx(tx: Transaction, keys: Seq[Key]): Map[Key, Entity] = {
    val entities = service.get(tx, keys.map(_.key).asJava).asScala.map {
      case (k, v) => Key(k) -> Entity(v)
    }
    entities.toMap
  }

  def get(keys: Seq[Key]): Map[Key, Entity] = {
    val got = service.get(keys.map(_.key).asJava)
    val entities = got.asScala.map {
      case (k, v) => Key(k) -> Entity(v)
    }
    entities.toMap
  }
}


sealed private[datastore] trait DatastoreGetProjectionListOps {
  self: DatastoreBase =>

  import scala.collection.JavaConverters._

  def getProjectionWithoutTx(keys: Seq[Key], properties: Map[String, Class[_]]): Map[Key, Entity] = getProjectionWithTx(null, keys, properties)

  def getProjectionWithTx(tx: Transaction, keys: Seq[Key], properties: Map[String, Class[_]]): Map[Key, Entity] = keys match {
    case Nil => Map.empty
    case _ =>
      val q = new LLQuery(keys.head.kind)
        .setFilter(new LLFilterPredicate("__key__", FilterOperator.IN, keys.map(_.key).asJava))
      properties.foreach { case (name, clazz) => q.addProjection(new PropertyProjection(name, clazz)) }

      val entities = service.prepare(tx, q).asList(FetchOptions.Builder.withLimit(keys.size)).asScala.toList
      assert(entities.size == keys.size)
      entities.map(e => Key(e.getKey) -> Entity(e)).toMap
  }

  def getProjection(keys: Seq[Key], properties: Map[String, Class[_]]): Map[Key, Entity] = keys match {
    case Nil => Map.empty
    case _ =>
      val q = new LLQuery(keys.head.kind)
        .setFilter(new LLFilterPredicate("__key__", FilterOperator.IN, keys.map(_.key).asJava))
      properties.foreach { case (name, clazz) => q.addProjection(new PropertyProjection(name, clazz)) }

      val entities = service.prepare(q).asList(FetchOptions.Builder.withLimit(keys.size)).asScala.toList
      assert(entities.size == keys.size)
      entities.map(e => Key(e.getKey) -> Entity(e)).toMap
  }
}

sealed private[datastore] trait DatastoreDeleteOps {
  self: DatastoreBase =>

  def deleteWithoutTx(key: Key): Unit = deleteWithTx(null, key)

  def deleteWithTx(tx: Transaction, key: Key): Unit = service.delete(tx, key.key)

  def delete(key: Key): Unit = service.delete(key.key)
}

sealed private[datastore] trait DatastoreDeleteListOps {
  self: DatastoreBase =>

  def deleteWithoutTx(keys: Seq[Key]): Unit = deleteWithTx(null, keys)

  def deleteWithTx(tx: Transaction, keys: Seq[Key]): Unit = service.delete(tx, keys.map(_.key).asJava)

  def delete(keys: Seq[Key]): Unit = service.delete(keys.map(_.key).asJava)
}

sealed private[datastore] trait DatastorePutOps extends DatastoreBase {

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

sealed private[datastore] trait DatastorePutListOps {
  self: DatastoreBase =>

  def putWithoutTx(entities: Seq[Entity]): Seq[Key] = putWithTx(null, entities)

  def putWithTx(tx: Transaction, entities: Seq[Entity]): Seq[Key] = {
    val e = entities.map(_.entity)
    service.put(tx, e.asJava).asScala.map(k => Key(k))
  }

  def put(entities: Seq[Entity]): Seq[Key] = {
    val e = entities.map(_.entity)
    service.put(e.asJava).asScala.map(k => Key(k))
  }
}

sealed private[datastore] trait DatastoreCreateOps {
  self: DatastoreBase with DatastorePutOps with DatastoreGetOptionOps =>


  def createWithoutTx(entity: Entity): Key = createWithTx(null, entity)

  def createWithTx(tx: Transaction, entity: Entity): Key = getOptionWithTx(tx, entity.key) match {
    case Some(_) => throw new ConcurrentModificationException("entity was already stored")
    case None => putWithTx(tx, entity)
  }

  def create(entity: Entity): Key = getOption(entity.key) match {
    case Some(_) => throw new ConcurrentModificationException("entity was already stored")
    case None => put(entity)
  }
}

sealed private[datastore] trait DatastoreCreateListOps {
  self: DatastoreBase with DatastorePutListOps with DatastoreGetListOps =>

  def createWithoutTx(entities: Seq[Entity]): Seq[Key] = createWithTx(null, entities)

  def createWithTx(tx: Transaction, entities: Seq[Entity]): Seq[Key] = {
    val keys = entities.map(_.key)
    if (getWithTx(tx, keys).exists(_._2 == null)) throw new ConcurrentModificationException("entity which is stored already is included")
    putWithTx(tx, entities)
  }

  def create(entities: Seq[Entity]): Seq[Key] = {
    val keys = entities.map(_.key)
    if (get(keys).nonEmpty) throw new ConcurrentModificationException("entity which is stored already is included")
    put(entities)
  }
}

sealed private[datastore] trait DatastoreUpdateOps {
  self: DatastoreBase with DatastorePutOps with DatastoreGetProjectionOps =>

  def updateWithoutTx(entity: Entity): Key = updateWithTx(null, entity)

  def updateWithTx(tx: Transaction, entity: Entity): Key = entity.versionProperty match {
    case Some(v) if entity.isSameVersion(getProjection(entity.key, Map(v.name -> classOf[java.lang.Long]))) => putWithTx(tx, entity)
    case Some(v) => throw new ConcurrentModificationException("invalid version property. %s ver. %d".format(entity.key, v.value))
    case None => putWithTx(tx, entity)
  }

  def update(entity: Entity): Key = entity.versionProperty match {
    case Some(v) if entity.isSameVersion(getProjection(entity.key, Map(v.name -> classOf[java.lang.Long]))) => put(entity)
    case Some(v) => throw new ConcurrentModificationException("invalid version property. %s ver. %d".format(entity.key, v.value))
    case None => put(entity)
  }
}

sealed private[datastore] trait DatastoreUpdateListOps {
  self: DatastoreBase with DatastorePutListOps with DatastoreGetProjectionListOps =>

  def updateWithoutTx(entities: Seq[Entity]): Seq[Key] = updateWithTx(null, entities)

  def updateWithTx(tx: Transaction, entities: Seq[Entity]): Seq[Key] = entities.head.versionProperty match {
    case Some(v) =>
      val got = getProjectionWithTx(tx, entities.map(_.key), Map(v.name -> classOf[java.lang.Long])).values.toSeq
      val invalids = getInvalidVersion(entities, got)
      if (invalids.nonEmpty) throw new ConcurrentModificationException("invalid version property.\n" + invalids.mkString("\n"))
      putWithTx(tx, entities)
    case None =>
      putWithTx(tx, entities)
  }

  def update(entities: Seq[Entity]): Seq[Key] = entities.head.versionProperty match {
    case Some(v) =>
      val got = getProjection(entities.map(_.key), Map(v.name -> classOf[java.lang.Long])).values.toSeq
      val invalids = getInvalidVersion(entities.sortBy(_.key), got.sortBy(_.key))
      if (invalids.nonEmpty) throw new ConcurrentModificationException("invalid version property. " + invalids)
      put(entities)
    case None =>
      put(entities)
  }

  private def getInvalidVersion(entity1: Seq[Entity], entity2: Seq[Entity]): Seq[String] = {
    assert(entity1.size == entity2.size)
    entity1.zip(entity2)
      .filterNot { case (e1, e2) => e1.isSameVersion(e2) }
      .map { case (e1, _) => "key:%s version:%d".format(e1.key, e1.version.get) }
  }
}

sealed private[datastore] trait DatastoreCreateKeyOps {
  self: DatastoreBase =>

  def createKey(kind: String, name: String): Key = Key(KeyFactory.createKey(kind, name))

  def createKey(kind: String, parent: Key, name: String): Key = Key(KeyFactory.createKey(parent.key, kind, name))

  def createKey(kind: String, id: Long): Key = Key(KeyFactory.createKey(kind, id))

  def createKey(kind: String, parent: Key, id: Long): Key = Key(KeyFactory.createKey(parent.key, kind, id))

  def allocateKey(kind: String): Key = Key(service.allocateIds(kind, 1).getStart)


  def allocateKeys(kind: String, count: Long): Seq[Key] = service.allocateIds(kind, count).asScala.map(Key(_)).toSeq

  def allocateKey(kind: String, parent: Key): Key = allocateKeys(kind, 1L).head

  def allocateKeys(kind: String, parent: Key, count: Long): Seq[Key] = service.allocateIds(parent.key, kind, count).asScala.map(Key(_)).toSeq
}

sealed private[datastore] trait DatastoreQueryOps {
  self: DatastoreBase =>

  def countWithTx(tx: Transaction, query: Query): Int = _count(Some(tx), query)

  def countWithoutTx(query: Query): Int = _count(Some(null), query)

  def count(query: Query): Int = _count(None, query)

  private def _count(tx: Option[Transaction], query: Query): Int = {
    val llQuery = _toLLQuery(query, keysOnly = true)
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

  def asSeqWithTx(tx: Transaction, query: Query): Seq[Entity] = _asSeq(Some(tx), query)

  def asSeqWithoutTx(query: Query): Seq[Entity] = _asSeq(Some(null), query)

  def asSeq(query: Query): Seq[Entity] = _asSeq(None, query)

  private def _asSeq(tx: Option[Transaction], query: Query): Seq[Entity] = {
    val q = query
    val options = (q.offset, q.limit) match {
      case (Some(o), Some(l)) => FetchOptions.Builder.withOffset(o).limit(l)
      case (Some(o), _) => FetchOptions.Builder.withOffset(o)
      case (_, Some(o)) => FetchOptions.Builder.withLimit(o)
      case _ => FetchOptions.Builder.withDefaults()
    }
    prepare(tx, q, keysOnly = false).asIterable(options).asScala.map {
      e => Entity(e)
    }.toSeq
  }

  def asSingleWithTx(tx: Transaction, query: Query): Entity = asSingleOptionWithTx(tx, query).getOrElse {
    throw new IllegalArgumentException(s"Entity not found for $query.")
  }

  def asSingleWithoutTx(query: Query): Entity = asSingleOptionWithoutTx(query).getOrElse {
    throw new IllegalArgumentException(s"Entity not found for $query.")
  }

  def asSingle(query: Query): Entity = asSingleOption(query).getOrElse {
    throw new IllegalArgumentException(s"Entity not found for $query.")
  }

  def asSingleOptionWithTx(tx: Transaction, query: Query): Option[Entity] = _asSingleOption(Some(tx), query)

  def asSingleOptionWithoutTx(query: Query): Option[Entity] = _asSingleOption(Some(null), query)

  def asSingleOption(query: Query): Option[Entity] = _asSingleOption(None, query)

  private def _asSingleOption(tx: Option[Transaction], query: Query): Option[Entity] = {
    val q = query
    Option(prepare(tx, q, keysOnly = false).asSingleEntity()).map(Entity(_))
  }

  def asKeySeqWithTx(tx: Transaction, query: Query): Seq[Key] = _asKeySeq(Some(tx), query)

  def asKeySeqWithoutTx(query: Query): Seq[Key] = _asKeySeq(Some(null), query)

  def asKeySeq(query: Query): Seq[Key] = _asKeySeq(None, query)

  private def _asKeySeq(tx: Option[Transaction], query: Query): Seq[Key] = {
    val q = query
    val options = (q.offset, q.limit) match {
      case (Some(o), Some(l)) => FetchOptions.Builder.withOffset(o).limit(l)
      case (Some(o), _) => FetchOptions.Builder.withOffset(o)
      case (_, Some(o)) => FetchOptions.Builder.withLimit(o)
      case _ => FetchOptions.Builder.withDefaults()
    }
    prepare(tx, q, keysOnly = true).asIterable(options).asScala.map {
      e => Key(e.getKey)
    }.toSeq
  }


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
        }, filters.map(f => _toLLFilter(f)).asJava)
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

sealed private[datastore] trait DatastoreTxOps {
  self: DatastoreBase =>

  def beginTx: Transaction = service.beginTransaction()

  def beginXgTx: Transaction = service.beginTransaction(TransactionOptions.Builder.withXG(true))

  def beginTx(option: TransactionOptions): Transaction = service.beginTransaction(option)

  def currentTx: Transaction = service.getCurrentTransaction

  def currentTx(tx: Transaction): Transaction = service.getCurrentTransaction(tx)

  def activeTx: Seq[Transaction] = service.getActiveTransactions.asScala.toSeq

  def tx[T](block: Transaction => T): T = {
    val tx = beginTx
    try {
      val ret = block(tx)
      tx.commit()
      ret
    } finally {
      if (tx.isActive) tx.rollback()
    }
  }

  def xgTx[T](block: Transaction => T): T = {
    val tx = beginXgTx
    try {
      val ret = block(tx)
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

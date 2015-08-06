package com.xhachi.gae4s.datastore

import com.google.appengine.api.datastore.Transaction

import scala.language.implicitConversions
import scala.reflect.ClassTag

trait EntityStoreContext {
  def ancestor: Option[Key[_]]
}

case class SimpleEntityStoreContext[T](key: Key[T]) extends EntityStoreContext {
  def ancestor: Option[Key[_]] = Some(key)
}

object NoAncestorEntityStoreContext extends EntityStoreContext {
  override def ancestor: Option[Key[_]] = None
}

trait EntityStoreBase {
  type ENTITY <: Entity[ENTITY]
  type Context = EntityStoreContext

  implicit def meta: EntityMeta[ENTITY]

  def datastore: Datastore = Datastore
}

abstract class EntityStore[E <: Entity[E] : ClassTag] extends EntityStoreBase with GettableStore {
  type ENTITY = E
}

trait GettableStore {
  self: EntityStoreBase =>
  type ENTITY <: Entity[ENTITY]

  def get(key: Key[ENTITY]): ENTITY = datastore.get(key)

  def getWithoutTx(key: Key[ENTITY]): ENTITY = datastore.getWithoutTx(key)

  def getWithTx(tx: Transaction, key: Key[ENTITY]): ENTITY = datastore.getWithTx(tx, key)

  def get(keys: Seq[Key[ENTITY]]): Map[Key[ENTITY], ENTITY] = datastore.get(keys)

  def getWithoutTx(keys: Seq[Key[ENTITY]]): Map[Key[ENTITY], ENTITY] = datastore.getWithoutTx(keys)

  def getWithTx(tx: Transaction, keys: Seq[Key[ENTITY]]): Map[Key[ENTITY], ENTITY] = datastore.getWithTx(tx, keys)

  def getOption(key: Key[ENTITY]): Option[ENTITY] = datastore.getOption(key)

  def getOptionWithoutTx(key: Key[ENTITY]): Option[ENTITY] = datastore.getOptionWithoutTx(key)

  def getOptionWithTx(tx: Transaction, key: Key[ENTITY]): Option[ENTITY] = datastore.getOptionWithTx(tx, key)

  def exists(key: Key[ENTITY]): Boolean = getOption(key).isDefined

  def existsWithoutTx(key: Key[ENTITY]): Boolean = getOptionWithoutTx(key).isDefined

  def existsWithTx(tx: Transaction, key: Key[ENTITY]): Boolean = getOptionWithTx(tx, key).isDefined

  implicit def toKeyedEntityStore(key: Key[ENTITY]): EntityStoreForKey = new EntityStoreForKey(key)

  implicit def toKeyedEntityStore(keys: Seq[Key[ENTITY]]): EntityStoreForKeys = new EntityStoreForKeys(keys)

  protected class EntityStoreForKey private[GettableStore](val key: Key[ENTITY]) {

    def get = GettableStore.this.get(key)

    def getWithoutTx = GettableStore.this.getWithoutTx(key)

    def getWithTx(tx: Transaction) = GettableStore.this.getWithTx(tx, key)

    def getOption = GettableStore.this.getOption(key)

    def getOptionWithoutTx = GettableStore.this.getOptionWithoutTx(key)

    def getOptionWithTx(tx: Transaction) = GettableStore.this.getOptionWithTx(tx, key)

    def exists = GettableStore.this.getOption(key).isDefined

    def existsWithoutTx = GettableStore.this.getOptionWithoutTx(key).isDefined

    def existsWithTx(tx: Transaction) = GettableStore.this.getOptionWithTx(tx, key).isDefined
  }

  protected class EntityStoreForKeys private[GettableStore](val keys: Seq[Key[ENTITY]]) {

    def get = GettableStore.this.get(keys)

    def getWithoutTx = GettableStore.this.getWithoutTx(keys)

    def getWithTx(tx: Transaction) = GettableStore.this.getWithTx(tx, keys)
  }

}


trait CreatableStore {
  self: EntityStoreBase with GettableStore =>

  type ENTITY <: Entity[ENTITY]

  def create(e: ENTITY) = datastore.create(e)

  def createWithoutTx(e: ENTITY) = datastore.createWithoutTx(e)

  def createWithTx(tx: Transaction, e: ENTITY) = datastore.createWithTx(tx, e)

  def create(e: Seq[ENTITY]) = datastore.create(e)

  def createWithoutTx(e: Seq[ENTITY]) = datastore.createWithoutTx(e)

  def createWithTx(tx: Transaction, e: Seq[ENTITY]) = datastore.createWithTx(tx, e)

  def createIfNotExists(key: Key[ENTITY], init: ENTITY => Unit = (e) => Unit) = {
    if (getOption(key).isEmpty) {
      val e = meta.createEntity(key)
      assert(key == e.key)
      init(e)
      create(e)
    }
  }

  def createIfNotExistsWithoutTx(key: Key[ENTITY], init: ENTITY => Unit = (e) => Unit) = {
    if (getOptionWithoutTx(key).isEmpty) {
      val e = meta.createEntity(key)
      init(e)
      createWithoutTx(e)
    }
  }

  def createIfNotExistsWithTx(tx: Transaction, key: Key[ENTITY], init: ENTITY => Unit = (e) => Unit) = {
    if (getOptionWithTx(tx, key).isEmpty) {
      val e = meta.createEntity(key)
      init(e)
      createWithTx(tx, e)
    }
  }

  implicit def toCreatableStoreFromEntity(entity: ENTITY): CreatableStoreForEntity = new CreatableStoreForEntity(entity)

  implicit def toCreatableStoreFromEntitySeq(entities: Seq[ENTITY]): CreatableStoreForEntities = new CreatableStoreForEntities(entities)

  protected class CreatableStoreForEntity private[CreatableStore](val entities: ENTITY) {

    def create() = CreatableStore.this.create(entities)

    def createWithoutTx() = CreatableStore.this.createWithoutTx(entities)

    def createWithTx(tx: Transaction) = CreatableStore.this.createWithTx(tx, entities)
  }

  protected class CreatableStoreForEntities private[CreatableStore](val entities: Seq[ENTITY]) {

    def create() = CreatableStore.this.create(entities)

    def createWithoutTx() = CreatableStore.this.createWithoutTx(entities)

    def createWithTx(tx: Transaction) = CreatableStore.this.createWithTx(tx, entities)
  }

}

trait UpdatableStore {
  self: EntityStoreBase =>

  def update(e: ENTITY): Key[ENTITY] = datastore.update(e)

  def updateWithoutTx(e: ENTITY): Key[ENTITY] = datastore.updateWithoutTx(e)

  def updateWithTx(tx: Transaction, e: ENTITY): Key[ENTITY] = datastore.updateWithTx(tx, e)

  def update(e: Seq[ENTITY]): Seq[Key[ENTITY]] = datastore.update(e)

  def updateWithoutTx(e: Seq[ENTITY]): Seq[Key[ENTITY]] = datastore.updateWithoutTx(e)

  def updateWithTx(tx: Transaction, e: Seq[ENTITY]): Seq[Key[ENTITY]] = datastore.updateWithTx(tx, e)

  implicit def toUpdatableStoreFromEntity(entity: ENTITY): UpdatableStoreForEntity = new UpdatableStoreForEntity(entity)

  implicit def toUpdatableStoreFromEntitySeq(entities: Seq[ENTITY]): UpdatableStoreForEntities = new UpdatableStoreForEntities(entities)

  protected class UpdatableStoreForEntity private[UpdatableStore](val entities: ENTITY) {

    def update(): Key[ENTITY] = UpdatableStore.this.update(entities)

    def updateWithoutTx(): Key[ENTITY] = UpdatableStore.this.updateWithoutTx(entities)

    def updateWithTx(tx: Transaction): Key[ENTITY] = UpdatableStore.this.updateWithTx(tx, entities)
  }

  protected class UpdatableStoreForEntities private[UpdatableStore](val entities: Seq[ENTITY]) {

    def update(): Seq[Key[ENTITY]] = UpdatableStore.this.update(entities)

    def updateWithoutTx(): Seq[Key[ENTITY]] = UpdatableStore.this.updateWithoutTx(entities)

    def updateWithTx(tx: Transaction): Seq[Key[ENTITY]] = UpdatableStore.this.updateWithTx(tx, entities)
  }

}

trait DeletableStore {
  self: EntityStoreBase =>

  def delete(key: Key[ENTITY]): Unit = datastore.delete(key)

  def deleteWithoutTx(key: Key[ENTITY]): Unit = datastore.deleteWithoutTx(key)

  def deleteWithTx(tx: Transaction, key: Key[ENTITY]): Unit = datastore.deleteWithTx(tx, key)

  def delete(keys: Seq[Key[ENTITY]]): Unit = datastore.delete(keys)

  def deleteWithoutTx(keys: Seq[Key[ENTITY]]): Unit = datastore.deleteWithoutTx(keys)

  def deleteWithTx(tx: Transaction, keys: Seq[Key[ENTITY]]): Unit = datastore.deleteWithTx(tx, keys)

  implicit def toKeyedDeletableStoreFromEntity(entity: ENTITY): DeletableStoreForKey = new DeletableStoreForKey(entity.key :: Nil)

  implicit def toKeyedDeletableStoreFromEntitySeq(entities: Seq[ENTITY]): DeletableStoreForKey = new DeletableStoreForKey(entities.map(_.key))

  implicit def toKeyedDeletableStoreFromKey(key: Key[ENTITY]): DeletableStoreForKey = new DeletableStoreForKey(key :: Nil)

  implicit def toKeyedDeletableStoreFromKeySeq(keys: Seq[Key[ENTITY]]): DeletableStoreForKey = new DeletableStoreForKey(keys)

  protected class DeletableStoreForKey private[DeletableStore](val key: Seq[Key[ENTITY]]) {

    def delete(): Unit = DeletableStore.this.delete(key)

    def deleteWithoutTx(): Unit = DeletableStore.this.deleteWithoutTx(key)

    def deleteWithTx(tx: Transaction): Unit = DeletableStore.this.deleteWithTx(tx, key)
  }

}

trait SingleStore extends IdentifiableKeyStore {
  self: EntityStoreBase with GettableStore with CreatableStore =>

  def createSingleKey(implicit context: Context): Key[ENTITY] = createKeyWithId(1)

  def createSingleIfNotExists(init: ENTITY => Unit = (e) => Unit)(implicit context: Context) = createIfNotExists(createSingleKey, init)

  def createSingleIfNotExistsWithoutTx(init: ENTITY => Unit = (e) => Unit)(implicit context: Context) = createIfNotExistsWithoutTx(createSingleKey, init)

  def createSingleIfNotExistsWithTx(tx: Transaction, init: ENTITY => Unit = (e) => Unit)(implicit context: Context) = createIfNotExistsWithTx(tx, createSingleKey, init)

  def getSingle(implicit context: Context): ENTITY = get(createSingleKey)

  def getSingleWithoutTx(implicit context: Context): ENTITY = getWithoutTx(createSingleKey)

  def getSingleWithTx(tx: Transaction)(implicit context: Context): ENTITY = getWithTx(tx, createSingleKey)

  def getOptionSingle(implicit context: Context): Option[ENTITY] = getOption(createSingleKey)

  def getOptionSingleWithoutTx(implicit context: Context): Option[ENTITY] = getOptionWithoutTx(createSingleKey)

  def getOptionSingleWithTx(tx: Transaction)(implicit context: Context): Option[ENTITY] = getOptionWithTx(tx, createSingleKey)
}

trait NamedStore {
  self: EntityStoreBase with GettableStore =>

  def createKeyWithName(name: String)(implicit context: Context) = context.ancestor match {
    case Some(p) => datastore.createKey[ENTITY](p, name)
    case None => datastore.createKey[ENTITY](name)
  }

  def createEntityWithName(name: String)(implicit context: Context) = meta.createEntity(createKeyWithName(name))

  def getByName(name: String)(implicit context: Context) = get(createKeyWithName(name))

  def getByNameWithoutTx(name: String)(implicit context: Context) = datastore.getWithoutTx(createKeyWithName(name))

  def getByNameWithTx(tx: Transaction, name: String)(implicit context: Context) = datastore.getWithTx(tx, createKeyWithName(name))

  def getOptionByName(name: String)(implicit context: Context) = getOption(createKeyWithName(name))

  def getOptionByNameWithoutTx(name: String)(implicit context: Context) = getOptionWithoutTx(createKeyWithName(name))

  def getOptionByNameWithTx(tx: Transaction, name: String)(implicit context: Context) = getOptionWithTx(tx, createKeyWithName(name))

  def getByNames(names: Seq[String])(implicit context: Context): Map[String, ENTITY] = datastore.get(names.map(createKeyWithName)).map {
    case (k, v) => k.name -> v
  }

  def getByNamesWithoutTx(names: Seq[String])(implicit context: Context): Map[String, ENTITY] = datastore.getWithoutTx(names.map(createKeyWithName)).map {
    case (k, v) => k.name -> v
  }

  def getByNamesWithTx(tx: Transaction, names: Seq[String])(implicit context: Context): Map[String, ENTITY] = datastore.getWithTx(tx, names.map(createKeyWithName)).map {
    case (k, v) => k.name -> v
  }
}

trait IdentifiableKeyStore {
  self: EntityStoreBase =>

  def createKeyWithId(id: Long)(implicit context: Context) = context.ancestor match {
    case Some(p) => datastore.createKey[ENTITY](p, id)
    case None => datastore.createKey[ENTITY](id)
  }

  def createEntityWithId(id: Long)(implicit context: Context) = meta.createEntity(createKeyWithId(id))

  def getById(id: Long)(implicit context: Context) = datastore.get(createKeyWithId(id))

  def getByIdWithoutTx(id: Long)(implicit context: Context) = datastore.getWithoutTx(createKeyWithId(id))

  def getByIdWithTx(tx: Transaction, id: Long)(implicit context: Context) = datastore.getWithTx(tx, createKeyWithId(id))

  def getOptionById(id: Long)(implicit context: Context) = datastore.getOption(createKeyWithId(id))

  def getOptionByIdWithoutTx(id: Long)(implicit context: Context) = datastore.getOptionWithoutTx(createKeyWithId(id))

  def getOptionByIdWithTx(tx: Transaction, id: Long)(implicit context: Context) = datastore.getOptionWithTx(tx, createKeyWithId(id))

  def getByIds(ids: Seq[Long])(implicit context: Context): Map[Long, ENTITY] = datastore.get(ids.map(createKeyWithId)).map {
    case (k, v) => k.id -> v
  }

  def getByIdsWithoutTx(ids: Seq[Long])(implicit context: Context): Map[Long, ENTITY] = datastore.getWithoutTx(ids.map(createKeyWithId)).map {
    case (k, v) => k.id -> v
  }

  def getByIdsWithTx(tx: Transaction, ids: Seq[Long])(implicit context: Context): Map[Long, ENTITY] = datastore.getWithTx(tx, ids.map(createKeyWithId)).map {
    case (k, v) => k.id -> v
  }
}

trait AllocatableKeyStore extends IdentifiableKeyStore {
  self: EntityStoreBase =>

  def allocateKey(implicit context: Context) = context.ancestor match {
    case Some(p) => datastore.allocateKey[ENTITY](p)
    case None => datastore.allocateKey[ENTITY]
  }

  def createEntityWithAllocatedKey(implicit context: Context) = meta.createEntity(allocateKey)

  def allocateKeys(count: Long)(implicit context: Context) = context.ancestor match {
    case Some(p) => datastore.allocateKeys[ENTITY](p, count)
    case None => datastore.allocateKeys[ENTITY](count)
  }
}

trait UUIDKeyStore extends NamedStore {
  self: EntityStoreBase with GettableStore =>

  import java.util.UUID

  def generateKey(implicit context: Context) = {
    var key: Key[ENTITY] = null
    while (key == null) {
      val created = createKeyWithName(UUID.randomUUID().toString)
      if (getOptionWithoutTx(created).isEmpty) key = created
    }
    key
  }

  def createEntityWithGeneratedKey(implicit context: Context) = meta.createEntity(generateKey)
}

trait QueryableStore {
  self: EntityStoreBase =>

  def query(query: Query[ENTITY] => Query[ENTITY]) = DatastoreQuery(datastore, query(Query(meta)))

  def query(ancestor: Key[_]) = DatastoreQuery(datastore, Query(meta, Some(ancestor)))

  def query(implicit context: EntityStoreContext) = DatastoreQuery(datastore, Query(meta, context.ancestor))

  def countWithTx(tx: Transaction) = datastore.countWithTx[ENTITY](tx)

  def countWithTx(tx: Transaction, query: Query[ENTITY]) = datastore.countWithTx(tx, query)

  def countWithoutTx = datastore.countWithoutTx[ENTITY]

  def countWithoutTx(query: Query[ENTITY]) = datastore.countWithoutTx(query)

  def count = datastore.count[ENTITY]

  def count(query: Query[ENTITY]) = datastore.count(query)

  def asSeqWithTx(tx: Transaction) = datastore.asSeqWithTx[ENTITY](tx)

  def asSeqWithTx(tx: Transaction, query: Query[ENTITY]) = datastore.asSeqWithTx(tx, query)

  def asSeqWithoutTx = datastore.asSeqWithoutTx[ENTITY]

  def asSeqWithoutTx(query: Query[ENTITY]) = datastore.asSeqWithoutTx(query)

  def asSeq = datastore.asSeq[ENTITY]

  def asSeq(query: Query[ENTITY]) = datastore.asSeq(query)

  def asKeySeqWithTx(tx: Transaction) = datastore.asKeySeqWithTx[ENTITY](tx)

  def asKeySeqWithTx(tx: Transaction, query: Query[ENTITY]) = datastore.asKeySeqWithTx(tx, query)

  def asKeySeqWithoutTx = datastore.asKeySeqWithoutTx[ENTITY]

  def asKeySeqWithoutTx(query: Query[ENTITY]) = datastore.asKeySeqWithoutTx(query)

  def asKeySeq = datastore.asKeySeq[ENTITY]

  def asKeySeq(query: Query[ENTITY]) = datastore.asKeySeq(query)

  def asSingleWithTx(tx: Transaction) = datastore.asSingleWithTx[ENTITY](tx)

  def asSingleWithTx(tx: Transaction, query: Query[ENTITY]) = datastore.asSingleWithTx(tx, query)

  def asSingleWithoutTx = datastore.asSingleWithoutTx[ENTITY]

  def asSingleWithoutTx(query: Query[ENTITY]) = datastore.asSingleWithoutTx(query)

  def asSingle = datastore.asSingle[ENTITY]

  def asSingle(query: Query[ENTITY]) = datastore.asSingle(query)

  def asSingleOptionWithTx(tx: Transaction) = datastore.asSingleOptionWithTx[ENTITY](tx)

  def asSingleOptionWithTx(tx: Transaction, query: Query[ENTITY]) = datastore.asSingleOptionWithTx(tx, query)

  def asSingleOptionWithoutTx = datastore.asSingleOptionWithoutTx[ENTITY]

  def asSingleOptionWithoutTx(query: Query[ENTITY]) = datastore.asSingleOptionWithoutTx(query)

  def asSingleOption(query: Query[ENTITY]) = datastore.asSingleOption(query)

  def asSingleOption = datastore.asSingleOption[ENTITY]


}



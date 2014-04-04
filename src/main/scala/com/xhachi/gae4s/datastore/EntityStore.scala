package com.xhachi.gae4s.datastore

import com.google.appengine.api.datastore.Transaction

trait EntityStoreBase {
  type ENTITY <: Entity[ENTITY]
  type META <: EntityMeta[ENTITY]

  protected implicit def meta: META

  protected def datastore: Datastore = Datastore

  def parentKey: Key[_] = null

  def parentKeyOption: Option[Key[_]] = parentKey match {
    case k: Key[_] => Some(k)
    case _ => None
  }
}

trait EntityStore[E <: Entity[E]] extends EntityStoreBase with GettableStore {
  type ENTITY = E
}

trait GettableStore extends EntityStoreBase {
  def get(key: Key[ENTITY]) = datastore.get(key)

  def getWithoutTx(key: Key[ENTITY]) = datastore.getWithoutTx(key)

  def getWithTx(tx: Transaction, key: Key[ENTITY]) = datastore.getWithTx(tx, key)

  def get(keys: Seq[Key[ENTITY]]) = datastore.get(keys)

  def getWithoutTx(keys: Seq[Key[ENTITY]]) = datastore.getWithoutTx(keys)

  def getWithTx(tx: Transaction, keys: Seq[Key[ENTITY]]) = datastore.getWithTx(tx, keys)

  def getOption(key: Key[ENTITY]): Option[ENTITY] = datastore.getOption(key)

  def getOptionWithoutTx(key: Key[ENTITY]): Option[ENTITY] = datastore.getOptionWithoutTx(key)

  def getOptionWithTx(tx: Transaction, key: Key[ENTITY]): Option[ENTITY] = datastore.getOptionWithTx(tx, key)

  def exists(key: Key[ENTITY]) = getOption(key).isDefined

  def existsWithoutTx(key: Key[ENTITY]) = getOptionWithoutTx(key).isDefined

  def existsWithTx(tx: Transaction, key: Key[ENTITY]) = getOptionWithTx(tx, key).isDefined

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


trait CreatableStore extends EntityStoreBase with GettableStore {
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

trait UpdatableStore extends EntityStoreBase {
  type ENTITY <: Entity[ENTITY]

  def update(e: ENTITY) = datastore.update(e)

  def updateWithoutTx(e: ENTITY) = datastore.updateWithoutTx(e)

  def updateWithTx(tx: Transaction, e: ENTITY) = datastore.updateWithTx(tx, e)

  def update(e: Seq[ENTITY]) = datastore.update(e)

  def updateWithoutTx(e: Seq[ENTITY]) = datastore.updateWithoutTx(e)

  def updateWithTx(tx: Transaction, e: Seq[ENTITY]) = datastore.updateWithTx(tx, e)

  implicit def toUpdatableStoreFromEntity(entity: ENTITY): UpdatableStoreForEntity = new UpdatableStoreForEntity(entity)

  implicit def toUpdatableStoreFromEntitySeq(entities: Seq[ENTITY]): UpdatableStoreForEntities = new UpdatableStoreForEntities(entities)

  protected class UpdatableStoreForEntity private[UpdatableStore](val entities: ENTITY) {

    def update() = UpdatableStore.this.update(entities)

    def updateWithoutTx() = UpdatableStore.this.updateWithoutTx(entities)

    def updateWithTx(tx: Transaction) = UpdatableStore.this.updateWithTx(tx, entities)
  }

  protected class UpdatableStoreForEntities private[UpdatableStore](val entities: Seq[ENTITY]) {

    def update() = UpdatableStore.this.update(entities)

    def updateWithoutTx() = UpdatableStore.this.updateWithoutTx(entities)

    def updateWithTx(tx: Transaction) = UpdatableStore.this.updateWithTx(tx, entities)
  }

}

trait DeletableStore extends EntityStoreBase {

  def delete(key: Key[ENTITY]) = datastore.delete(key)

  def deleteWithoutTx(key: Key[ENTITY]) = datastore.deleteWithoutTx(key)

  def deleteWithTx(tx: Transaction, key: Key[ENTITY]) = datastore.deleteWithTx(tx, key)

  def delete(keys: Seq[Key[ENTITY]]) = datastore.delete(keys)

  def deleteWithoutTx(keys: Seq[Key[ENTITY]]) = datastore.deleteWithoutTx(keys)

  def deleteWithTx(tx: Transaction, keys: Seq[Key[ENTITY]]) = datastore.deleteWithTx(tx, keys)

  implicit def toKeyedDeletableStoreFromEntity(entity: ENTITY): DeletableStoreForKey = new DeletableStoreForKey(entity.key :: Nil)

  implicit def toKeyedDeletableStoreFromEntitySeq(entities: Seq[ENTITY]): DeletableStoreForKey = new DeletableStoreForKey(entities.map(_.key))

  implicit def toKeyedDeletableStoreFromKey(key: Key[ENTITY]): DeletableStoreForKey = new DeletableStoreForKey(key :: Nil)

  implicit def toKeyedDeletableStoreFromKeySeq(keys: Seq[Key[ENTITY]]): DeletableStoreForKey = new DeletableStoreForKey(keys)

  protected class DeletableStoreForKey private[DeletableStore](val key: Seq[Key[ENTITY]]) {

    def delete() = DeletableStore.this.delete(key)

    def deleteWithoutTx() = DeletableStore.this.deleteWithoutTx(key)

    def deleteWithTx(tx: Transaction) = DeletableStore.this.deleteWithTx(tx, key)
  }

}

trait SingleStore extends IdentifiableKeyStore with CreatableStore {
  type ENTITY <: Entity[ENTITY]

  def createSingleKey: Key[ENTITY] = createKey(1)

  def createSingleIfNotExists(init: ENTITY => Unit = (e) => Unit) = createIfNotExists(createSingleKey, init)

  def createSingleIfNotExistsWithoutTx(init: ENTITY => Unit = (e) => Unit) = createIfNotExistsWithoutTx(createSingleKey, init)

  def createSingleIfNotExistsWithTx(tx: Transaction, init: ENTITY => Unit = (e) => Unit) = createIfNotExistsWithTx(tx, createSingleKey, init)

  def getSingle: ENTITY = get(createSingleKey)

  def getSingleWithoutTx: ENTITY = getWithoutTx(createSingleKey)

  def getSingleWithTx(tx: Transaction): ENTITY = getWithTx(tx, createSingleKey)

  def getOptionSingle: Option[ENTITY] = getOption(createSingleKey)

  def getOptionSingleWithoutTx: Option[ENTITY] = getOptionWithoutTx(createSingleKey)

  def getOptionSingleWithTx(tx: Transaction): Option[ENTITY] = getOptionWithTx(tx, createSingleKey)
}

trait NamedStore extends EntityStoreBase with GettableStore {
  type ENTITY <: Entity[ENTITY]

  def createKey(name: String) = parentKeyOption match {
    case Some(p) => datastore.createKey[ENTITY, META](p, name)
    case _ => datastore.createKey[ENTITY, META](name)
  }

  def getByName(name: String) = get(createKey(name))

  def getByNameWithoutTx(name: String) = datastore.getWithoutTx(createKey(name))

  def getByNameWithTx(tx: Transaction, name: String) = datastore.getWithTx(tx, createKey(name))

  def getOptionByName(name: String) = getOption(createKey(name))

  def getOptionByNameWithoutTx(name: String) = getOptionWithoutTx(createKey(name))

  def getOptionByNameWithTx(tx: Transaction, name: String) = getOptionWithTx(tx, createKey(name))

  def getByNames(names: Seq[String]): Map[String, ENTITY] = datastore.get(names.map(createKey)).map {
    case (k, v) => k.name -> v
  }

  def getByNamesWithoutTx(names: Seq[String]): Map[String, ENTITY] = datastore.getWithoutTx(names.map(createKey)).map {
    case (k, v) => k.name -> v
  }

  def getByNamesWithTx(tx: Transaction, names: Seq[String]): Map[String, ENTITY] = datastore.getWithTx(tx, names.map(createKey)).map {
    case (k, v) => k.name -> v
  }
}

trait IdentifiableKeyStore extends EntityStoreBase {

  def createKey(id: Long) = parentKeyOption match {
    case Some(p) => datastore.createKey[ENTITY, META](p, id)
    case None => datastore.createKey[ENTITY, META](id)
  }

  def getById(id: Long) = datastore.get(createKey(id))

  def getByIdWithoutTx(id: Long) = datastore.getWithoutTx(createKey(id))

  def getByIdWithTx(tx: Transaction, id: Long) = datastore.getWithTx(tx, createKey(id))

  def getOptionById(id: Long) = datastore.getOption(createKey(id))

  def getOptionByIdWithoutTx(id: Long) = datastore.getOptionWithoutTx(createKey(id))

  def getOptionByIdWithTx(tx: Transaction, id: Long) = datastore.getOptionWithTx(tx, createKey(id))

  def getByIds(ids: Seq[Long]): Map[Long, ENTITY] = datastore.get(ids.map(createKey)).map {
    case (k, v) => k.id -> v
  }

  def getByIdsWithoutTx(ids: Seq[Long]): Map[Long, ENTITY] = datastore.getWithoutTx(ids.map(createKey)).map {
    case (k, v) => k.id -> v
  }

  def getByIdsWithTx(tx: Transaction, ids: Seq[Long]): Map[Long, ENTITY] = datastore.getWithTx(tx, ids.map(createKey)).map {
    case (k, v) => k.id -> v
  }
}

trait AllocatableKeyStore extends IdentifiableKeyStore {

  def allocateKey = parentKeyOption match {
    case Some(p) => datastore.allocateKey[ENTITY, META](p)
    case None => datastore.allocateKey[ENTITY, META]()
  }

  def allocateKeys(count: Long) = parentKeyOption match {
    case Some(p) => datastore.allocateKeys[ENTITY, META](p, count)
    case None => datastore.allocateKeys[ENTITY, META](count)
  }
}

trait UUIDKeyStore extends NamedStore {

  import java.util.UUID

  def generateKey = {
    var key: Key[ENTITY] = null
    while (key == null) {
      val created = createKey(UUID.randomUUID().toString)
      if (getOptionWithoutTx(created).isEmpty) key = created
    }
    key
  }
}

trait QueryableStore extends EntityStoreBase {

  def query: Query[ENTITY, META] = parentKeyOption match {
    case Some(p) => datastore.query[ENTITY, META](p)
    case None => datastore.query[ENTITY, META]
  }

  def queryWithoutTx: Query[ENTITY, META] = parentKeyOption match {
    case Some(p) => datastore.queryWithoutTx[ENTITY, META](p)
    case None => datastore.queryWithoutTx[ENTITY, META]
  }

  def queryWithTx(tx: Transaction): Query[ENTITY, META] = parentKeyOption match {
    case Some(p) => datastore.queryWithTx[ENTITY, META](tx, p)
    case None => datastore.queryWithTx[ENTITY, META](tx)
  }
}

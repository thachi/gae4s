package com.xhachi.gae4s.datastore

import com.google.appengine.api.datastore.Transaction

trait EntityStoreBase {
  protected type ENTITY <: Entity[ENTITY]
  protected type META <: EntityMeta[ENTITY]

  protected implicit def meta: META

  protected def datastore: Datastore = Datastore

  def parentKey: Key[_] = null

  def parentKeyOption: Option[Key[_]] = parentKey match {
    case k: Key[_] => Some(k)
    case _ => None
  }

  def create(e: ENTITY) = datastore.create(e)

  def get(key: Key[ENTITY]) = datastore.get(key)

  def getOption(key: Key[ENTITY]): Option[ENTITY] = datastore.getOption(key)

  def getOrElse(key: Key[ENTITY], default: => ENTITY): ENTITY = getOption(key) match {
    case Some(e) => e
    case None => default
  }

  def getWithoutTx(key: Key[ENTITY]) = datastore.getWithoutTx(key)

  def getOptionWithoutTx(key: Key[ENTITY]): Option[ENTITY] = datastore.getOptionWithoutTx(key)

  def getOrElseWithoutTx(key: Key[ENTITY], default: => ENTITY): ENTITY = getOptionWithoutTx(key) match {
    case Some(e) => e
    case None => default
  }

  def getWithTx(tx: Transaction, key: Key[ENTITY]) = datastore.getWithTx(tx, key)

  def getOptionWithTx(tx: Transaction, key: Key[ENTITY]): Option[ENTITY] = datastore.getOptionWithTx(tx, key)

  def getOrElseWithTx(tx: Transaction, key: Key[ENTITY], default: => ENTITY): ENTITY = getOptionWithTx(tx, key) match {
    case Some(e) => e
    case None => default
  }
}

trait EntityStore[E <: Entity[E]] extends EntityStoreBase {
  protected type ENTITY = E
}

trait SingleStore extends IdentifiableKeyStore {

  def createSingleKey: Key[ENTITY] = createKey(1)

  def getSingle: ENTITY = get(createSingleKey)

  def getSingleWithoutTx: ENTITY = getWithoutTx(createSingleKey)

  def getSingleWithTx(tx: Transaction): ENTITY = getWithTx(tx, createSingleKey)
}

trait NamedStore extends EntityStoreBase {

  def createKey(name: String) = parentKeyOption match {
    case Some(p) => datastore.createKey[ENTITY, META](p, name)
    case _ => datastore.createKey[ENTITY, META](name)
  }

  def getByName(name: String) = get(createKey(name))

  def getOptionByName(name: String) = getOption(createKey(name))

  def getOrElseByName(name: String, default: => ENTITY) = getOrElse(createKey(name), default)

  def getByNameWithoutTx(name: String) = datastore.getWithoutTx(createKey(name))

  def getByNameWithTx(tx: Transaction, name: String) = datastore.getWithTx(tx, createKey(name))

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

trait UpdatableStore extends EntityStoreBase {

  def update(e: ENTITY) = datastore.update(e)

  def updateWithoutTx(e: ENTITY) = datastore.updateWithoutTx(e)

  def updateWithTx(tx: Transaction, e: ENTITY) = datastore.updateWithTx(tx, e)

  def update(e: Seq[ENTITY]) = datastore.update(e)

  def updateWithoutTx(e: Seq[ENTITY]) = datastore.updateWithoutTx(e)

  def updateWithTx(tx: Transaction, e: Seq[ENTITY]) = datastore.updateWithTx(tx, e)
}

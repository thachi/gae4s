package com.xhachi.gae4s.datastore

trait EntityStoreBase {
  protected type ENTITY <: Entity[ENTITY]
  protected type META <: EntityMeta[ENTITY]

  protected implicit def meta: META

  protected def datastore: Datastore

  val parentKey: Key[_]

  lazy val parentKeyOption: Option[Key[_]] = parentKey match {
    case k: Key[_] => Some(k)
    case _ => None
  }

  def create(e: ENTITY) = datastore.create(e)

  def get(key: Key[ENTITY]) = datastore.get(key)
}

trait EntityStore[E <: Entity[E]] extends EntityStoreBase {
  protected type ENTITY = E
}

trait SingleStore extends IdentifiableKeyStore {

  def createSingleKey: Key[ENTITY] = createKey(1)

  def getSingle: ENTITY = datastore.get(createSingleKey)
}

trait NamedStore extends EntityStoreBase {

  def createKey(name: String) = parentKeyOption match {
    case Some(p) => datastore.createKey[ENTITY, META](p, name)
    case _ => datastore.createKey[ENTITY, META](name)
  }

  def get(name: String) = datastore.get(createKey(name))
}

trait IdentifiableKeyStore extends EntityStoreBase {

  def createKey(id: Long) = parentKeyOption match {
    case Some(p) => datastore.createKey[ENTITY, META](p, id)
    case None => datastore.createKey[ENTITY, META](id)
  }

  def get(id: Long) = datastore.get(createKey(id))
}

trait AllocatableKeyStore extends IdentifiableKeyStore {
  def allocateKey = parentKeyOption match {
    case Some(p) => datastore.allocateKey[ENTITY, META](p)
    case None => datastore.allocateKey[ENTITY, META]()
  }
}


trait QueryableStore extends EntityStoreBase {
  def query: Query[ENTITY, META] = parentKeyOption match {
    case Some(p) => datastore.query[ENTITY, META](p)
    case None => datastore.query[ENTITY, META]
  }
}

trait UpdatableStore extends EntityStoreBase {
  def update(e: ENTITY) = datastore.update(e)
}
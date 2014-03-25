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
}

trait EntityStore[E <: Entity[E]] extends EntityStoreBase {
  protected type ENTITY = E

}

trait DescendantEntityStore[E <: Entity[E], P <: Entity[P]]
  extends EntityStoreBase {

}

trait KeyedStore extends EntityStoreBase {
  def get(key: Key[ENTITY]) = datastore.get(key)
}

trait SingleStore extends IdentifiableKeyStore {

  def createSingleKey: Key[ENTITY] = createKey(1)

  def getSingle: ENTITY = datastore.get(createSingleKey)
}

trait NamedStore extends KeyedStore {
  def createKey(name: String) = parentKeyOption match {
    case Some(p) => datastore.createKey[ENTITY, META](p, name)
    case _ => datastore.createKey[ENTITY, META](name)
  }
}

trait IdentifiableKeyStore extends KeyedStore {
  def createKey(id: Long) = parentKeyOption match {
    case Some(p) => datastore.createKey[ENTITY, META](p, id)
    case None => datastore.createKey[ENTITY, META](id)
  }
}

trait AutoAllocateKeyStore extends KeyedStore {
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
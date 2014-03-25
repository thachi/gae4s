package com.xhachi.gae4s.datastore


import com.google.appengine.api.datastore.{Key => LLKey, KeyFactory}
import KeyFactory._

class Key[E] private(private[datastore] val key: LLKey) {

  val kind: String = key.getKind

  val name = key.getName match {
    case n: String => Some(n)
    case _ => None
  }

  val id = key.getId match {
    case 0 => None
    case i => Some(i)
  }

  def parent[P <: Entity[P]]: Option[Key[P]] = key.getParent match {
    case p: LLKey => Some(Key[P](p))
    case _ => None
  }

  override def equals(o: Any) = o match {
    case o: Key[E] => key.equals(o.key)
  }
  override def hashCode = key.hashCode
  override def toString = key.toString
}

object Key {
  def apply[E <: Entity[E]](key: LLKey) = new Key[E](key)

  def apply[E <: Entity[E]](name: String)(implicit meta: EntityMeta[E]) = {
    val key = createKey(meta.kind, name)
    new Key[E](key)
  }

  def apply[E <: Entity[E]](id: Long)(implicit meta: EntityMeta[E]) = {
    new Key[E](createKey(meta.kind, id))
  }

  def apply[E <: Entity[E]](parent: Key[_], name: String)(implicit meta: EntityMeta[E]) = {
    val key = createKey(parent.key, meta.kind, name)
    new Key[E](key)
  }

  def apply[E <: Entity[E]](parent: Key[_], id: Long)(implicit meta: EntityMeta[E]) = {
    val key = createKey(parent.key, meta.kind, id)
    new Key[E](key)
  }

  def fromKeyStrong[E <: Entity[E]](keyString: String): Key[E] = Key[E](stringToKey(keyString))

  def toKeyStrong(key: Key[_]): String = keyToString(key.key)

}


/*
sealed trait Key[E] {

  def toLLKey: LLKey

  def parentKey: Option[Key[_]]

}

object Key {

  def fromLLKey[E](key: LLKey) = key

  def toKey[E](key: LLKey): Key[E] = key match {
    case k if k.getName != null => toNamedKey(key)
    case _ => toIdentifiedKey(key)
  }

  def toNamedKey[E](key: LLKey): NamedKey[E] = {
    if (key.getName == null) throw new IllegalArgumentException("key has no name")
    key.getParent match {
      case p: LLKey => {
        val parentKey = Some(Key[_](p))
        NamedKey[E](key.getKind, key.getName, parentKey)
      }
      case _ => NamedKey[E](key.getKind, key.getName)
    }
  }

  def toIdentifiedKey[E](key: LLKey): IdentifiedKey[E] = {
    if (key.getId == 0) throw new IllegalArgumentException("key has no id")
    key.getParent match {
      case p: LLKey => IdentifiedKey[E](key.getKind, key.getId, Some(Key[_](p)))
      case _ => IdentifiedKey[E](key.getKind, key.getId)
    }
  }
}

case class NamedKey[E](kind: String, name: String, parentKey: Option[Key[_]] = None) extends Key[E] {

  def toLLKey: LLKey = parentKey match {
    case Some(p) => new LLKey(kind, name, p.toLLKey)
    case None => new LLKey(kind, name)
  }
}

case class IdentifiedKey[E](kind: String, id: Long, parentKey: Option[Key[_]] = None) extends Key[E] {

  def toLLKey: LLKey = parentKey match {
    case Some(p) => new LLKey(kind, id, p.toLLKey)
    case None => new LLKey(kind, id)
  }
}

*/
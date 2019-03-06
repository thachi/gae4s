package com.xhachi.gae4s.datastore


import com.google.appengine.api.datastore.{Key => LLKey, KeyFactory}

case class Key(key: LLKey) extends Ordered[Key] {

  def kind: String = key.getKind

  def nameOption: Option[String] = Option(key.getName)

  def name: String = nameOption.getOrElse(throw new IllegalStateException("name is not defined."))

  def id: Long = idOption.getOrElse(throw new IllegalStateException("id is not defined."))

  def idOption: Option[Long] = Option(key.getId).flatMap {
    case 0 => None
    case i => Some(i)
  }

  def parent: Option[Key] = Option(key.getParent).map(Key(_))

  override def equals(other: Any) = other match {
    case o: Key => key.equals(o.key)
    case _ => false
  }

  override def hashCode: Int = key.hashCode

  override def toString: String = key.toString

  def toWebSafeString: String = KeyFactory.keyToString(key)

  override def compare(that: Key): Int = this.key.compareTo(that.key)

  def withParent(parent: Key): Key = idOption match {
    case Some(i) => Key(key.getKind, i, Some(parent))
    case None => Key(key.getKind, name, Some(parent))
  }
}

object Key {

  def fromWebSafeString(string: String): Key = Key(KeyFactory.stringToKey(string))

  def apply(kind: String, id: Long, parent: Option[Key]): Key = parent match {
    case Some(p) => Key(KeyFactory.createKey(p.key, kind, id))
    case None => Key(KeyFactory.createKey(kind, id))
  }

  def apply(kind: String, name: String, parent: Option[Key]): Key = parent match {
    case Some(p) => Key(KeyFactory.createKey(p.key, kind, name))
    case None => Key(KeyFactory.createKey(kind, name))
  }
}
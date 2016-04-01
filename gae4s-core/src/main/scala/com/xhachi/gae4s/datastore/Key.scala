package com.xhachi.gae4s.datastore


import com.google.appengine.api.datastore.{Key => LLKey, KeyFactory}

case class Key(key: LLKey) extends Ordered[Key] {

  val kind: String = key.getKind

  val name = key.getName

  val nameOption = name match {
    case n: String => Some(n)
    case _ => None
  }

  val id: Long = key.getId

  val idOption = id match {
    case 0 => None
    case i => Some(i)
  }

  def parent: Option[Key] = key.getParent match {
    case p: LLKey => Some(Key(p))
    case _ => None
  }

  override def equals(other: Any) = other match {
    case o: Key => key.equals(o.key)
    case _ => false
  }

  override def hashCode = key.hashCode

  override def toString = key.toString

  def toWebSafeString = KeyFactory.keyToString(key)

  override def compare(that: Key): Int = this.key.compareTo(that.key)
}

object Key {

  def fromWebSafeString(string: String): Key = Key(KeyFactory.stringToKey(string))

  def apply(kind: String, id: Long): Key = Key(KeyFactory.createKey(kind, id))

  def apply(kind: String, name: String): Key = Key(KeyFactory.createKey(kind, name))

}
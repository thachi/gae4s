package com.xhachi.gae4s.datastore


import com.google.appengine.api.datastore.{Key => LLKey, KeyFactory}

case class Key[E] private(key: LLKey) extends Ordered[Key[E]] with Serializable {

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

  def parent[P <: Entity[P]]: Option[Key[P]] = key.getParent match {
    case p: LLKey => Some(Key[P](p))
    case _ => None
  }

  override def equals(other: Any) = other match {
    case o: Key[E] => key.equals(o.key)
    case _ => false
  }

  override def hashCode = key.hashCode

  override def toString = key.toString

  def toWebSafeString = KeyFactory.keyToString(key)

  override def compare(that: Key[E]): Int = this.key.compareTo(that.key)
}

object Key {
  def fromWebSafeString[E](string: String) = Key[E](KeyFactory.stringToKey(string))
}
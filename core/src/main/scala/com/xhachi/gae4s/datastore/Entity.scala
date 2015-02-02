package com.xhachi.gae4s.datastore

import com.google.appengine.api.datastore.{Key => LLKey}


abstract class Entity[E <: Entity[E]] extends Serializable {

  val key: Key[E]

  final def keyOption: Option[Key[E]] = key match {
    case k: Key[E] => Some(k)
    case _ => None
  }

}

/**
 * TODO: ちゃんとしたエラーチェックの仕組みをDatastoreに入れたい
 * @tparam A 親のキー
 */
trait Ancestor[A <: Entity[A]] {
  self: Entity[_] =>
  assert(key.key.getParent != null)

  def parentKey = key.key.getParent match {
    case k: LLKey => Key[A](k)
    case _ => throw new IllegalStateException("this entity has no parent.")
  }
}

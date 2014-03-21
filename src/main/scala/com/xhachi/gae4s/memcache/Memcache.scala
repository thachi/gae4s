package com.xhachi.gae4s.memcache

import com.google.appengine.api.memcache.{Expiration, MemcacheService, MemcacheServiceFactory}
import java.util.Date

import scala.collection.JavaConversions._
import com.google.appengine.api.memcache.MemcacheService.SetPolicy._
import com.google.appengine.api.memcache.MemcacheService._

/**
 * Class to access Memcache service.
 *
 * @author Takashi Hachinohe
 * @param service the MemcacheService instance
 */
class Memcache private[Memcache](service: MemcacheService) {

  def namespace = service.getNamespace

  def put[K](key: K, value: Any, expire: Option[Date] = None, policy: SetPolicy = SET_ALWAYS): Boolean = expire match {
    case Some(date) => service.put(key, value, Expiration.onDate(date), policy)
    case _ => service.put(key, value, null, policy)
  }

  def putAll[K](values: Map[K, Any], expire: Option[Date] = None, policy: SetPolicy = SET_ALWAYS): Set[K] = expire match {
    case Some(date) => service.putAll(values, Expiration.onDate(date), policy).toSet
    case _ => service.putAll(values, null, policy).toSet
  }

  def putIfUntouched[K, V](key: K, oldValue: IdValue[V], newValue: V, expire: Option[Date] = None): Boolean = expire match {
    case Some(date) => service.putIfUntouched(key, oldValue.identifiableValue, newValue, Expiration.onDate(date))
    case _ => service.putIfUntouched(key, oldValue.identifiableValue, newValue, null)
  }


  def delete(key: AnyRef, millisNoReAdd: Long = 0L): Boolean = service.delete(key, millisNoReAdd)

  def deleteAll[K](keys: Seq[K], millisNoReAdd: Long = 0L): Set[K] = service.deleteAll(keys, millisNoReAdd).toSet

  def contains(key: AnyRef): Boolean = service.contains(key)

  def get[V](key: AnyRef): Option[V] = service.get(key) match {
    case value: Any => Some(value.asInstanceOf[V])
    case null => None
  }

  def getAll[T](keys: Seq[AnyRef]): Map[AnyRef, Option[T]] = service.getAll(keys).map {
    case (k, v: Any) => k -> Some(v.asInstanceOf[T])
    case (k, null) => k -> None
  }.toMap

  def getIdentifiable[T](key: AnyRef): IdValue[T] = service.getIdentifiable(key) match {
    case value: IdentifiableValue => new IdValue[T](value)
    case null => null
  }

  def getIdentifiables[K, V](key: Seq[K]): Map[K, IdValue[V]] =
    service.getIdentifiables(key).map {
      case (k, v) => k -> new IdValue[V](v)
    }.toMap

  def clearAll() = service.clearAll()

  def inclement(key: AnyRef, delta: Long = 1L, initialValue: Option[Long] = None): Long = initialValue match {
    case Some(i) => service.increment(key, delta, i)
    case None => service.increment(key, delta)
  }

  def inclementAll[T](keys: Seq[T], delta: Long = 1L, initialValue: Option[Long] = None): Map[T, Long] = initialValue match {
    case Some(i) => service.incrementAll(keys, delta, i).toMap.asInstanceOf[Map[T, Long]]
    case None => service.incrementAll(keys, delta).toMap.asInstanceOf[Map[T, Long]]
  }

  def statistics = service.getStatistics

  class IdValue[T](private[Memcache] val identifiableValue: IdentifiableValue) {
    def value: T = {
      val value1: AnyRef = {
        identifiableValue.getValue
      }
      value1.asInstanceOf[T]
    }
  }

}

/**
 * Object to access default Memcache service.
 *
 * @author Takashi Hachinohe
 */
object Memcache extends Memcache(MemcacheServiceFactory.getMemcacheService) {

  def apply(name: String) = new Memcache(MemcacheServiceFactory.getMemcacheService(name))

}

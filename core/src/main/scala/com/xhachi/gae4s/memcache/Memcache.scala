package com.xhachi.gae4s.memcache

import java.util.Date

import com.google.appengine.api.memcache.MemcacheService.SetPolicy._
import com.google.appengine.api.memcache.MemcacheService._
import com.google.appengine.api.memcache.{Expiration, MemcacheService, MemcacheServiceFactory}

import scala.collection.JavaConversions._

/**
 * Class to access Memcache service.
 *
 * @author Takashi Hachinohe
 * @param service the MemcacheService instance
 */
class Memcache private[Memcache](service: MemcacheService, policy: SetPolicy = SET_ALWAYS) extends Serializable {

  def namespace = service.getNamespace

  def put[K](key: K, value: Any): Boolean = service.put(key, value, null, policy)

  def put[K](key: K, value: Any, expirationDate: Date): Boolean = service.put(key, value, Expiration.onDate(expirationDate), policy)

  def put[K](key: K, value: Any, lifeTimeSecond: Int): Boolean = service.put(key, value, Expiration.byDeltaSeconds(lifeTimeSecond), policy)

  def putAll[K](values: Map[K, Any]): Set[K] = service.putAll(values, null, policy).toSet

  def putAll[K](values: Map[K, Any], expirationDate: Date): Set[K] = service.putAll(values, Expiration.onDate(expirationDate), policy).toSet

  def putAll[K](values: Map[K, Any], lifeTimeSecond: Int): Set[K] = service.putAll(values, Expiration.byDeltaSeconds(lifeTimeSecond), policy).toSet

  def putIfUntouched[K, V](key: K, oldValue: IdValue[V], newValue: V): Boolean = service.putIfUntouched(key, oldValue.identifiableValue, newValue, null)

  def putIfUntouched[K, V](key: K, oldValue: IdValue[V], newValue: V, expirationDate: Date): Boolean = service.putIfUntouched(key, oldValue.identifiableValue, newValue, Expiration.onDate(expirationDate))

  def putIfUntouched[K, V](key: K, oldValue: IdValue[V], newValue: V, lifeTimeSecond: Int): Boolean = service.putIfUntouched(key, oldValue.identifiableValue, newValue, Expiration.byDeltaSeconds(lifeTimeSecond))


  def delete(key: AnyRef, millisNoReAdd: Long = 0L): Boolean = service.delete(key, millisNoReAdd)

  def deleteAll[K](keys: Seq[K], millisNoReAdd: Long = 0L): Set[K] = service.deleteAll(keys, millisNoReAdd).toSet

  def contains(key: AnyRef): Boolean = service.contains(key)

  def get[V](key: AnyRef): Option[V] = service.get(key) match {
    case value: Any => Some(value.asInstanceOf[V])
    case null => None
  }

  def getOrElse[V](key: AnyRef, default: => V): V = service.get(key) match {
    case value: Any => value.asInstanceOf[V]
    case null => default
  }

  def getOrElseUpdate[V](key: AnyRef)(default: => V): V = service.get(key) match {
    case value: Any => value.asInstanceOf[V]
    case null =>
      put(key, default)
      default
  }

  def getOrElseUpdate[V](key: AnyRef, expirationDate: Date)(default: => V): V = service.get(key) match {
    case value: Any => value.asInstanceOf[V]
    case null =>
      put(key, default, expirationDate)
      default
  }

  def getOrElseUpdate[V](key: AnyRef, lifeTimeSecond: Int)(default: => V): V = service.get(key) match {
    case value: Any => value.asInstanceOf[V]
    case null =>
      put(key, default, lifeTimeSecond)
      default
  }

  def getAll[T](keys: Seq[AnyRef]): Map[AnyRef, Option[T]] = service.getAll(keys).map {
    case (k, v: Any) => k -> Some(v.asInstanceOf[T])
    case (k, null) => k -> None
  }.toMap

  def getIdentifiable[T](key: AnyRef): Option[IdValue[T]] = service.getIdentifiable(key) match {
    case value: IdentifiableValue => Some(new IdValue[T](value))
    case null => None
  }

  def getAllIdentifiable[K, V](key: Seq[K]): Map[K, Option[IdValue[V]]] =
    service.getIdentifiables(key).map {
      case (k, v: Any) => k -> Some(new IdValue[V](v))
      case (k, _) => k -> None
    }.toMap

  def clearAll() = service.clearAll()

  def inclement(key: AnyRef, delta: Long = 1L, initialValue: Option[Long] = None): Long = initialValue match {
    case Some(i) => service.increment(key, delta, i)
    case None => service.increment(key, delta, 0L)
  }

  def inclementAll[T](keys: Seq[T], delta: Long = 1L, initialValue: Option[Long] = None): Map[T, Long] = initialValue match {
    case Some(i) => service.incrementAll(keys, delta, i).toMap.asInstanceOf[Map[T, Long]]
    case None => service.incrementAll(keys, delta).toMap.asInstanceOf[Map[T, Long]]
  }

  def statistics = service.getStatistics


}

class IdValue[T](private[memcache] val identifiableValue: IdentifiableValue) {
  def value: T = {
    val value1: AnyRef = {
      identifiableValue.getValue
    }
    value1.asInstanceOf[T]
  }
}

/**
 * Object to access default Memcache service.
 *
 * @author Takashi Hachinohe
 */
object Memcache extends Memcache(MemcacheServiceFactory.getMemcacheService, SET_ALWAYS) {

  def apply(name: String, policy: SetPolicy = SET_ALWAYS) = new Memcache(MemcacheServiceFactory.getMemcacheService(name), policy)

  def apply(policy: SetPolicy) = new Memcache(MemcacheServiceFactory.getMemcacheService, policy)

}


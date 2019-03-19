package com.xhachi.gae4s.memcache

import java.util.Date

import com.google.appengine.api.memcache.MemcacheService.SetPolicy._
import com.google.appengine.api.memcache.MemcacheService._
import com.google.appengine.api.memcache.{Expiration, MemcacheService, MemcacheServiceFactory, Stats}

import scala.collection.JavaConverters._
import scala.concurrent.duration.Duration

/**
  * Class to access Memcache service.
  *
  * @author Takashi Hachinohe
  * @param service the MemcacheService instance
  */
class Memcache private[Memcache](service: MemcacheService, policy: SetPolicy = SET_ALWAYS) {

  def namespace: String = service.getNamespace

  def put[K](key: K, value: Any): Boolean = service.put(key, value, null, policy)

  def put[K](key: K, value: Any, expirationDate: Date): Boolean = service.put(key, value, Expiration.onDate(expirationDate), policy)

  def put[K](key: K, value: Any, duration: Duration): Boolean = service.put(key, value, Expiration.byDeltaMillis(duration.toMillis.toInt), policy)

  def putAll[K](values: Map[K, Any]): Set[K] = service.putAll(values.asJava, null, policy).asScala.toSet

  def putAll[K](values: Map[K, Any], expirationDate: Date): Set[K] = service.putAll(values.asJava, Expiration.onDate(expirationDate), policy).asScala.toSet

  def putAll[K](values: Map[K, Any], duration: Duration): Set[K] = service.putAll(values.asJava, Expiration.byDeltaMillis(duration.toMillis.toInt), policy).asScala.toSet

  def putIfUntouched[K, V](key: K, oldValue: IdValue[V], newValue: V): Boolean = service.putIfUntouched(key, oldValue.identifiableValue, newValue, null)

  def putIfUntouched[K, V](key: K, oldValue: IdValue[V], newValue: V, expirationDate: Date): Boolean = service.putIfUntouched(key, oldValue.identifiableValue, newValue, Expiration.onDate(expirationDate))

  def putIfUntouched[K, V](key: K, oldValue: IdValue[V], newValue: V, duration: Duration): Boolean = service.putIfUntouched(key, oldValue.identifiableValue, newValue, Expiration.byDeltaMillis(duration.toMillis.toInt))


  def delete(key: AnyRef, millisNoReAdd: Long = 0L): Boolean = service.delete(key, millisNoReAdd)

  def deleteAll[K](keys: Seq[K], millisNoReAdd: Long = 0L): Set[K] = service.deleteAll(keys.asJava, millisNoReAdd).asScala.toSet

  def contains(key: AnyRef): Boolean = service.contains(key)

  def get[V](key: AnyRef): Option[V] = Option(service.get(key)).map(_.asInstanceOf[V])

  def getOrElse[V](key: AnyRef, default: => V): V = Option(service.get(key)).map(_.asInstanceOf[V]).getOrElse(default)

  def getOrElseUpdate[V](key: AnyRef)(default: => V): V = Option(service.get(key)) match {
    case Some(value) => value.asInstanceOf[V]
    case None =>
      val d: V = default
      put(key, d)
      d
  }

  def getOrElseUpdate[V](key: AnyRef, expirationDate: Date)(default: => V): V = Option(service.get(key)) match {
    case Some(value) => value.asInstanceOf[V]
    case None =>
      put(key, default, expirationDate)
      default
  }

  def getOrElseUpdate[V](key: AnyRef, duration: Duration)(default: => V): V = Option(service.get(key)) match {
    case Some(value) => value.asInstanceOf[V]
    case None =>
      put(key, default, duration)
      default
  }

  def getAll[K, T](keys: Seq[K]): Map[K, Option[T]] = {
    val all = service.getAll(keys.asJava).asScala
    keys.map {
      case k if all.contains(k) => k -> all.get(k).map(_.asInstanceOf[T])
      case k => k -> None
    }.toMap
  }

  def getIdentifiable[T](key: AnyRef): Option[IdValue[T]] = Option(service.getIdentifiable(key)).map(new IdValue[T](_))

  def getAllIdentifiable[K, V](key: Seq[K]): Map[K, Option[IdValue[V]]] =
    service.getIdentifiables(key.asJava).asScala.map {
      case (k, v: Any) => k -> Some(new IdValue[V](v))
      case (k, _) => k -> None
    }.toMap

  def clearAll(): Unit = service.clearAll()

  def inclement(key: AnyRef, delta: Long = 1L, initialValue: Option[Long] = None): Long = {
    val i = initialValue.getOrElse(0L)
    service.increment(key, delta, i)
  }

  def inclementAll[T](keys: Seq[T], delta: Long = 1L, initialValue: Option[Long] = None): Map[T, Long] = {
    val i = initialValue.getOrElse(0L)
    service.incrementAll(keys.asJava, delta, i).asScala.toMap.asInstanceOf[Map[T, Long]]
  }

  def statistics: Stats = service.getStatistics
}

class IdValue[T](private[memcache] val identifiableValue: IdentifiableValue) {
  def value: T = {
    val value1: AnyRef = identifiableValue.getValue
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


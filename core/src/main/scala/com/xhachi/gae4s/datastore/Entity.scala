package com.xhachi.gae4s.datastore

import java.util.Date

import com.google.appengine.api.datastore.{Entity => LLEntity, Key => LLKey, Query => LLQuery}

import scala.collection.JavaConverters._


object Entity {
  def apply(e: LLEntity): Entity = Entity(
    Key(e.getKey),
    e.getProperties.asScala.toSeq.map {
      case (VersionProperty.name, v) => VersionProperty(v.asInstanceOf[Long])
      case (CreationDateProperty.name, v) => CreationDateProperty(v.asInstanceOf[Date])
      case (ModificationDateProperty.name, v) => ModificationDateProperty(v.asInstanceOf[Date])
      case (k, v: LLKey) if !e.isUnindexedProperty(k) => IndexedProperty(k, Key(v))
      case (k, v) if !e.isUnindexedProperty(k) => IndexedProperty(k, v)
      case (k, v: LLKey) => UnindexedProperty(k, Key(v))
      case (k, v) => UnindexedProperty(k, v)
    }
  )
}

/**
  * エンティティを表します。
  */
case class Entity(key: Key, properties: Seq[Property[_]] = Seq.empty) {

  def apply[T](name: String): T = {
    val find: Option[Property[_]] = properties.find(_.name == name)
    val map: Option[T] = find.map(_.value.asInstanceOf[T])
    map.get
  }

  def get[T](name: String): Option[T] = properties.find(_.name == name).map(_.value.asInstanceOf[T])

  def set[T](name: String, value: T): Entity = Entity(
    key,
    properties.map {
      case p: Property[T] if p.name == name => p.withValue(value)
      case p => p
    }
  )

  protected[datastore] def entity: LLEntity = {
    val e = new LLEntity(key.key)
    properties.foreach {
      case UnindexedProperty(name, value: Key) => e.setProperty(name, value.key)
      case UnindexedProperty(name, value) => e.setProperty(name, value)
      case p => e.setIndexedProperty(p.name, p.value)
    }
    e
  }

  def version: Option[Long] = versionProperty.map(_.value)

  def versionProperty = properties.find(_.isInstanceOf[VersionProperty]).map(_.asInstanceOf[VersionProperty])

  def isSameKind(other: Entity) = key == other.key

  def isSameVersion(other: Entity) = isSameKind(other) && {
    (version, other.version) match {
      case (Some(v1), Some(v2)) => v1 == v2
      case _ => true
    }
  }
}


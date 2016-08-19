package com.xhachi.gae4s.datastore

import com.google.appengine.api.datastore.{Entity => LLEntity, Key => LLKey, Query => LLQuery}

import scala.collection.JavaConverters._


object Entity {
  def apply(e: LLEntity): Entity = Entity(
    Key(e.getKey),
    e.getProperties.asScala.toSeq.map {
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
      case UnindexedProperty(name, value: Key) => e.setUnindexedProperty(name, value.key)
      case UnindexedProperty(name, value: Seq[_]) => e.setUnindexedProperty(name, value.asJava)
      case UnindexedProperty(name, value) => e.setUnindexedProperty(name, value)
      case IndexedProperty(name, value: Key) => e.setIndexedProperty(name, value.key)
      case IndexedProperty(name, value: Seq[_]) => e.setIndexedProperty(name, value.asJava)
      case IndexedProperty(name, value) => e.setIndexedProperty(name, value)
      case VersionProperty(name, value) => e.setIndexedProperty(name, value + 1)
    }
    e
  }

  def version: Option[Long] = versionProperty.map(_.value)

  def versionProperty = properties.find(_.isInstanceOf[VersionProperty]).map(_.asInstanceOf[VersionProperty])

  def isSameKind(other: Entity) = key == other.key

  def isSameVersion(other: Entity) = isSameKind(other) && {
    (version, versionProperty.flatMap(v => other.get[Long](v.name))) match {
      case (Some(v1), Some(v2)) => v1 == v2
      case _ => true
    }
  }

  def versioned(name: String): Entity = Entity(
    this.key,
    this.properties.map{
      case p if p.name == name => VersionProperty(p.name, p.value.asInstanceOf[Long])
      case p => p
    }
  )

}


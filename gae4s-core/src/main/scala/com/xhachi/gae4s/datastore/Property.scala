package com.xhachi.gae4s.datastore

sealed trait Property[T] {
  def name: String

  def value: T

  def withValue(value: T): this.type
}

case class UnindexedProperty[T](name: String, value: T) extends Property[T] {
  def withValue(value: T) = UnindexedProperty[T](name, value).asInstanceOf[this.type]
}

case class IndexedProperty[T](name: String, value: T) extends Property[T] {
  def withValue(value: T) = IndexedProperty[T](name, value).asInstanceOf[this.type]
}

case class VersionProperty(name: String, value: Long) extends Property[Long] {
  def withValue(value: Long) = VersionProperty(name, value).asInstanceOf[this.type]
}

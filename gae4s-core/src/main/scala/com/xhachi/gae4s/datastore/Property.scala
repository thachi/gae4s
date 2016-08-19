package com.xhachi.gae4s.datastore

sealed trait Property[T] {
  def name: String

  def value: T

  def withValue(value: T): this.type
}

sealed abstract class NamedProperty[T](val name: String) extends Property[T]

case class UnindexedProperty[T](name: String, value: T) extends Property[T] {
  override def withValue(value: T) = UnindexedProperty[T](name, value).asInstanceOf[this.type]
}

case class IndexedProperty[T](name: String, value: T) extends Property[T] {
  override def withValue(value: T) = IndexedProperty[T](name, value).asInstanceOf[this.type]
}


object VersionProperty {
  val name = "version"
}

case class VersionProperty(value: Long) extends NamedProperty[Long](VersionProperty.name) {
  override def withValue(value: Long) = VersionProperty(value).asInstanceOf[this.type]
}

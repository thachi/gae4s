package com.xhachi.gae4s.datastore

import com.xhachi.gae4s.datastore.annotations.entity

import scala.reflect.ClassTag


@entity
abstract class JsonDataEntity[E <: JsonDataEntity[E, D], D <: AnyRef : ClassTag] extends Entity[E] {
  var data: D
}


@entity
abstract class SerializableDataEntity[E <: SerializableDataEntity[E, D], D <: Serializable : ClassTag] extends Entity[E] {
  var data: D
}

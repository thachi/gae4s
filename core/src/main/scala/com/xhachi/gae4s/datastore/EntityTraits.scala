package com.xhachi.gae4s.datastore

import java.util.Date

import com.xhachi.gae4s.datastore.meta._

trait MutableEntity extends Version with UpdatedAt with CreatedAt {
  self: Entity[_] =>
}

trait ImmutableEntity extends CreatedAt {
  self: Entity[_] =>
}

@entity
trait Version {
  self: Entity[_] =>

  @property(version = true)
  var version: Long = 0L
}

@entity
trait CreatedAt {
  self: Entity[_] =>

  @property(creationDate = true)
  var createdAt: Date = null
}

@entity
trait UpdatedAt {
  self: Entity[_] =>

  @property(modificationDate = true)
  var updatedAt: Date = null
}


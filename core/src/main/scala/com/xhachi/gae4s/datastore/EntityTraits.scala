package com.xhachi.gae4s.datastore

import java.util.Date

import com.xhachi.gae4s.datastore.meta._

@entity
trait Version {
  self: Entity[_] =>

  @property(version = true, order = 1000003)
  var version: Long = 0L
}

@entity
trait CreatedAt {
  self: Entity[_] =>

  @property(creationDate = true, order = 1000002)
  var createdAt: Date = null
}

@entity
trait UpdatedAt {
  self: Entity[_] =>

  @property(modificationDate = true, order = 1000001)
  var updatedAt: Date = null
}


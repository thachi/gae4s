package com.xhachi.gae4s.datastore

import java.util.Date

import com.xhachi.gae4s.datastore.annotations.{creationDate, entity, modificationDate, version}

@entity
trait Mutable extends Version with UpdatedAt with CreatedAt {
  self: Entity[_] =>
}

@entity
trait Immutable extends CreatedAt {
  self: Entity[_] =>
}

@entity(version = "version")
trait Version {
  self: Entity[_] =>

  @version
  var version: Long = 0L
}

@entity(version = "", creationDate = "createdAt")
trait CreatedAt {
  self: Entity[_] =>

  @creationDate
  var createdAt: Date = null
}

@entity(version = "", creationDate = "", modificationDate = "updatedAt")
trait UpdatedAt {
  self: Entity[_] =>

  @modificationDate
  var updatedAt: Date = null
}

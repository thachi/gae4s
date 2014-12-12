package com.xhachi.gae4s.datastore

import java.util.Date

import scala.collection.mutable.ListBuffer

trait ApplyProperty {
  type LLEntity = com.google.appengine.api.datastore.Entity
  type Entity
  type ApplyPropertyFrom = (LLEntity, Entity) => Unit
  type ApplyPropertyTo = (Entity, LLEntity) => Unit

  private val applyFromLLEntityBuffer: ListBuffer[ApplyPropertyFrom] = ListBuffer()

  private val applyToLLEntityBuffer: ListBuffer[ApplyPropertyTo] = ListBuffer()

  protected def addApplyFromLLEntity(f: ApplyPropertyFrom) = applyFromLLEntityBuffer += f

  protected def addApplyToLLEntity(f: ApplyPropertyTo) = applyToLLEntityBuffer += f

  protected def applyFromLLEntity(from: LLEntity, to: Entity) = {
    applyFromLLEntityBuffer.toSeq.foreach(_(from, to))
  }

  protected def applyToLLEntity(from: Entity, to: LLEntity) = {
    applyToLLEntityBuffer.toSeq.foreach(_(from, to))
  }

}


trait CreatedAt {
  self: Entity[_] =>

  final var createdAt: Option[Date] = None

}

trait CreatedAtMeta extends ApplyProperty {

  type Entity <: CreatedAt

  final val createdAt = new OptionProperty(new DateProperty("createdAt") with IndexedProperty[Date])

  addApplyToLLEntity {
    (from: Entity, to: LLEntity) => createdAt.setValueToLLEntity(to)(from.createdAt match {
      case Some(d: Date) => Some(d)
      case _ => Some(new Date())
    })
  }

  addApplyFromLLEntity {
    (from: LLEntity, to: Entity) => to.createdAt = createdAt.getValueFromLLEntity(from)
  }
}


trait UpdatedAt {
  self: Entity[_] =>

  final var updatedAt: Option[Date] = None

}

trait UpdatedAtMeta extends ApplyProperty {
  type Entity <: UpdatedAt

  final val updatedAt = new OptionProperty(new DateProperty("updatedAt") with IndexedProperty[Date])

  addApplyToLLEntity {
    (from: Entity, to: LLEntity) => updatedAt.setValueToLLEntity(to)(Some(new Date()))
  }

  addApplyFromLLEntity {
    (from: LLEntity, to: Entity) => to.updatedAt = updatedAt.getValueFromLLEntity(from)
  }
}

trait Version {
  self: Entity[_] =>

  final var version: Long = 0L

}

trait VersionMeta extends ApplyProperty {
  type Entity <: Version

  final val version = new LongProperty("version") with IndexedProperty[Long]

  addApplyToLLEntity {
    (from: Entity, to: LLEntity) =>
      version.setValueToLLEntity(to)(from.version + 1L)
  }

  addApplyFromLLEntity {
    (from: LLEntity, to: Entity) =>
      to.version = version.getValueFromLLEntity(from)
  }
}

trait Mutable extends CreatedAt with UpdatedAt with Version {
  self: Entity[_] =>
}

trait MutableMeta extends CreatedAtMeta with UpdatedAtMeta with VersionMeta {
  type Entity <: Mutable
}

trait Immutable extends CreatedAt {
  self: Entity[_] =>
}

trait ImmutableMeta extends CreatedAtMeta {
  type Entity <: Immutable
}

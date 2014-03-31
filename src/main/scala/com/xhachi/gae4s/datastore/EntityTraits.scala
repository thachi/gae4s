package com.xhachi.gae4s.datastore

import java.util.Date
import com.google.appengine.api.datastore.{Entity => LLEntity}
import scala.collection.mutable.ListBuffer

trait ApplyProperty {
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

  final var createdAt: Option[Date] = None

}

trait CreatedAtMeta extends ApplyProperty {
  type Entity <: CreatedAt

  final val createdAt = new OptionProperty(new DateProperty("createdAt"))

  addApplyToLLEntity {
    (from: Entity, to: LLEntity) => createdAt.setToStore(from.createdAt match {
      case Some(d: Date) => Some(d)
      case _ => Some(new Date())
    })(to)
  }

  addApplyFromLLEntity {
    (from: LLEntity, to: Entity) => to.createdAt = createdAt.getFromStore(from)
  }
}


trait UpdatedAt {
  type E <: UpdatedAt

  final var updatedAt: Option[Date] = None

}

trait UpdatedAtMeta extends ApplyProperty {
  type Entity <: UpdatedAt

  final val updatedAt = new OptionProperty(new DateProperty("updatedAt"))

  addApplyToLLEntity {
    (from: Entity, to: LLEntity) => updatedAt.setToStore(Some(new Date()))(to)
  }

  addApplyFromLLEntity {
    (from: LLEntity, to: Entity) => to.updatedAt = updatedAt.getFromStore(from)
  }
}

trait Version {

  final var version: Long = 0L

}

trait VersionMeta extends ApplyProperty {
  type Entity <: Version

  final val version = new LongProperty("version")

  addApplyToLLEntity {
    (from: Entity, to: LLEntity) =>
      version.setToStore(from.version + 1L)(to)
  }

  addApplyFromLLEntity {
    (from: LLEntity, to: Entity) =>
      to.version = version.getFromStore(from)
  }
}

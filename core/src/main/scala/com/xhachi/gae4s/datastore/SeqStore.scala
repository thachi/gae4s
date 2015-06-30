package com.xhachi.gae4s.datastore


class SeqStore[E <: Entity[E]](entities: Seq[E])(implicit meta: EntityMeta[E]) {

  def query: SeqQuery[E] = SeqQuery(this, Query[E])

  def count(query: Query[E]): Int = asSeq(query).size

  def asSeq(query: Query[E]): Seq[E] = {
    val filtered = entities.filter {
      entity =>
        (entity.keyOption.map(_.parent).flatten, query.ancestorOption, query.filterOption) match {
          case (Some(parentKey), Some(ancestorKey), _) if parentKey != ancestorKey =>
            false
          case (_, _, Some(filter)) =>
            filter.isMatch(entity, meta)
          case _ =>
            true
        }
    }
    val sorted = filtered.sortWith {
      case (e1, e2) =>
        query.sorts.map(_.lt(e1, e2, meta)).contains(true)
    }
    sorted
  }

  def asSingle(query: Query[E]): E = asSingleOption(query).getOrElse(throw new IllegalArgumentException(s"Entity not found for $query."))

  def asSingleOption(query: Query[E]): Option[E] = asSeq(query) match {
    case head :: Nil => Some(head)
    case _ => None
  }

  def asKeySeq(query: Query[E]): Seq[Key[E]] = asSeq(query).map(_.key)
}

object SeqStore {
  def apply[E <: Entity[E]](seq: Seq[E])(implicit meta: EntityMeta[E]) = new SeqStore(seq)
}

object SeqQuery {
  //  implicit def toSeqQuery[E <: Entity[E]](query: Query[E]): SeqQuery[E] = SeqQuery(query)
}

case class SeqQuery[E <: Entity[E]](store: SeqStore[E], query: Query[E]) {

  def asSeq = store.asSeq(query)

  def asKeySeq = store.asKeySeq(query)

  def asSingle = store.asSingle(query)

  def asSingleOption = store.asSingleOption(query)

  def count = store.count(query)

}
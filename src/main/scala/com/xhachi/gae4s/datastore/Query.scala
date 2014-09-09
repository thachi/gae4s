package com.xhachi.gae4s.datastore

import scala.collection.JavaConversions._
import com.google.appengine.api.datastore.Query.{Filter => LLFilter}
import com.google.appengine.api.datastore.Query.{FilterPredicate => LLFilterPredicate}
import com.google.appengine.api.datastore.Query.{SortDirection => LLSortDirection}
import com.google.appengine.api.datastore.Query.{SortPredicate => LLSortPredicate}
import com.google.appengine.api.datastore.Query.{CompositeFilter => LLCompositeFilter}
import com.google.appengine.api.datastore.Query.{CompositeFilterOperator, SortDirection, FilterOperator}
import com.google.appengine.api.datastore.{Query => LLQuery, EntityNotFoundException, Transaction}

case class Query[E <: Entity[E], M <: EntityMeta[E]] private[datastore](
                                                                         datastore: DatastoreQueryMethods,
                                                                         meta: M,
                                                                         tx: Option[Transaction],
                                                                         ancestorOption: Option[Key[_]] = None,
                                                                         filterOption: Option[Filter] = None,
                                                                         sorts: Seq[Sort] = Nil
                                                                         ) {

  def ancestor(ancestor: Key[_]): Query[E, M] = copy(ancestorOption = Some(ancestor))

  def filter(filters: (M => Filter)): Query[E, M] = copy(filterOption = Some(filters(meta)))

  def sort(sort: (M => Sort), sorts: (M => Sort)*): Query[E, M] = copy(sorts = sort(meta) :: sorts.map(_(meta)).toList)

  def count: Int = datastore.count(this)

  def asSeq: Seq[E] = datastore.asSeq(this)

  def asSingle: E = datastore.asSingle(this)

  def asSingleOption: Option[E] = datastore.asSingleOption(this)

  def asKeySeq: Seq[Key[E]] = datastore.asKeySeq(this)

  def count(entities: Seq[E]): Int = asSeq(entities).size

  def asSeq(entities: Seq[E]): Seq[E] = entities.filter {
    entity =>
      (entity.keyOption, ancestorOption, filterOption) match {
        case (Some(entityKey), Some(ancestorKey), _) if entityKey != ancestorKey => false
        case (_, _, Some(filter)) => filter.isMatch(entity, meta)
        case _ => true
      }
  }

  def asSingle(entities: Seq[E]): E = asSingleOption.getOrElse(throw new IllegalArgumentException("entity not found."))

  def asSingleOption(entities: Seq[E]): Option[E] = asSeq(entities) match {
    case head :: Nil => Some(head)
    case _ => None
  }

  def asKeySeq(entities: Seq[E]): Seq[Key[E]] = asSeq(entities).map(_.key)

  private[datastore] def toLLQuery(keysOnly: Boolean): LLQuery = {
    val query = new LLQuery(meta.kind)
    if (keysOnly) query.setKeysOnly() else query.clearKeysOnly()
    if (ancestorOption.isDefined) query.setAncestor(ancestorOption.get.key)
    if (filterOption.isDefined) query.setFilter(filterOption.get.toLLFilter)
    sorts.foreach(s => query.addSort(s.name, s.direction))
    query
  }
}


trait Filter {

  private[datastore] def toLLFilter: LLFilter

  def isMatch[E <: Entity[E]](entity: E, meta: EntityMeta[E]): Boolean

  def &&(f: Filter): Filter = {
    CompositeFilterPredicate(CompositeFilterOperator.AND, this :: f :: Nil)
  }

  def ||(f: Filter): Filter = {
    CompositeFilterPredicate(CompositeFilterOperator.OR, this :: f :: Nil)
  }
}

case class FilterPredicate(name: String, operator: FilterOperator, value: Any) extends Filter {

  private[datastore] def toLLFilter = (operator, value) match {
    case (FilterOperator.IN, value: Seq[_]) => new LLFilterPredicate(name, operator, seqAsJavaList(value))
    case _ => new LLFilterPredicate(name, operator, value)
  }

  def isMatch[E <: Entity[E]](entity: E, meta: EntityMeta[E]): Boolean = {
    val e = meta.toLLEntity(entity)
    import FilterOperator._
    (operator, e.getProperty(name), value) match {
      case (LESS_THAN, Some(v1: Ordered[Any]), Some(v2: Ordered[Any])) =>
        v1 < v2
      case (LESS_THAN_OR_EQUAL, Some(v1: Ordered[Any]), Some(v2: Ordered[Any]))
      => v1 <= v2
      case (GREATER_THAN, Some(v1: Ordered[Any]), Some(v2: Ordered[Any])) =>
        v1 > v2
      case (GREATER_THAN_OR_EQUAL, Some(v1: Ordered[Any]), Some(v2: Ordered[Any])) =>
        v1 >= v2
      case (  EQUAL, v1, v2) =>
        v1 == v2
      case (NOT_EQUAL, v1, v2) =>
        v1 != v2
      case (IN, v, values: Seq[_]) =>
        values.contains(v)
      case _ => false
    }
  }
}

case class CompositeFilterPredicate(operator: CompositeFilterOperator, filters: Seq[Filter]) extends Filter {

  private[datastore] def toLLFilter = new LLCompositeFilter(operator, filters.map(_.toLLFilter))

  override def isMatch[E <: Entity[E]](entity: E, meta: EntityMeta[E]): Boolean = {
    import CompositeFilterOperator._
    operator match {
      case AND =>
        filters.map(f => f.isMatch(entity, meta)).filter(m => !m).isEmpty
      case OR =>
        filters.map(f => f.isMatch(entity, meta)).filter(m => m).nonEmpty
    }
  }
}

trait Sort {

  private[datastore] def name: String

  private[datastore] def direction: LLSortDirection
}

case class SortPredicate(name: String, direction: SortDirection) extends Sort {

  private[datastore] def toLLSortDirection = new LLSortPredicate(name, direction)
}

package com.xhachi.gae4s.datastore

import com.google.appengine.api.datastore.Query.{CompositeFilter => LLCompositeFilter, CompositeFilterOperator, Filter => LLFilter, FilterOperator, FilterPredicate => LLFilterPredicate, SortDirection => LLSortDirection, SortPredicate => LLSortPredicate}
import com.google.appengine.api.datastore.{Query => LLQuery, Transaction}

import scala.collection.JavaConversions._
import scala.language.experimental.macros


case class Query[E <: Entity[E]] private[datastore](datastore: DatastoreQueryMethods,
                                                    meta: EntityMeta[E],
                                                    tx: Option[Transaction],
                                                    ancestorOption: Option[Key[_]] = None,
                                                    filterOption: Option[Filter] = None,
                                                    sorts: Seq[Sort] = Nil,
                                                    offset: Option[Int] = None,
                                                    limit: Option[Int] = None) {

  def ancestor(ancestor: Key[_]): Query[E] = copy(ancestorOption = Some(ancestor))

  def filter(filter: E => Boolean): Query[E] = macro EntityMacro.filter[E]

  def sort(sort: E => Any): Query[E] = macro EntityMacro.sortBy[E]

  //  def filter(filters: (meta.type => Filter)): Query[E] = copy(filterOption = Some(filters(meta)))

  //  def sort(sort: (meta.type => Sort), sorts: (meta.type => Sort)*): Query[E] = copy(sorts = sort(meta) :: sorts.map(_(meta)).toList)

  def offset(o: Int): Query[E] = copy(offset = Some(o))

  def limit(l: Int): Query[E] = copy(limit = Some(l))

  def count: Int = datastore.count(this)

  def asSeq: Seq[E] = datastore.asSeq(this)

  def asSingle: E = datastore.asSingle(this)

  def asSingleOption: Option[E] = datastore.asSingleOption(this)

  def asKeySeq: Seq[Key[E]] = datastore.asKeySeq(this)

  def count(entities: Seq[E]): Int = asSeq(entities).size

  def asSeq(entities: Seq[E]): Seq[E] = {
    val filtered = entities.filter {
      entity =>
        (entity.keyOption.map(_.parent).flatten, ancestorOption, filterOption) match {
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
        sorts.map(_.lt(e1, e2, meta)).contains(true)
    }
    sorted
  }

  def asSingle(entities: Seq[E]): E = asSingleOption.getOrElse(null.asInstanceOf[E])

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

  private[datastore] def isMatch[E <: Entity[E]](entity: E, meta: EntityMeta[E]): Boolean

  def &&(f: Filter): Filter = {
    CompositeFilterPredicate(CompositeFilterOperator.AND, this :: f :: Nil)
  }

  def ||(f: Filter): Filter = {
    CompositeFilterPredicate(CompositeFilterOperator.OR, this :: f :: Nil)
  }
}

case class FilterPredicate[T](name: String, operator: FilterOperator, property: IndexedProperty[T], value: T, values: Seq[T] = Nil) extends Filter {

  import com.google.appengine.api.datastore.Query.FilterOperator._

  if (operator != IN && values.nonEmpty) throw new IllegalArgumentException("valid values")

  private[datastore] def toLLFilter = operator match {
    case FilterOperator.IN => new LLFilterPredicate(name, operator, seqAsJavaList(value +: values))
    case _ => new LLFilterPredicate(name, operator, property.toStoreProperty(value))
  }

  private[datastore] def isMatch[E <: Entity[E]](entity: E, meta: EntityMeta[E]): Boolean = {
    val v: T = property.fromStoreProperty(meta.toLLEntity(entity).getProperty(name))

    operator match {
      case EQUAL => property.compare(v, value) == 0
      case NOT_EQUAL => property.compare(v, value) != 0
      case LESS_THAN => property.compare(v, value) < 0
      case LESS_THAN_OR_EQUAL => property.compare(v, value) <= 0
      case GREATER_THAN => 0 < property.compare(v, value)
      case GREATER_THAN_OR_EQUAL => 0 <= property.compare(v, value)
      case IN => (value +: values).contains(v)
      case _ => false
    }
  }
}

case class CompositeFilterPredicate(operator: CompositeFilterOperator, filters: Seq[Filter]) extends Filter {

  private[datastore] def toLLFilter = new LLCompositeFilter(operator, filters.map(_.toLLFilter))

  private[datastore] def isMatch[E <: Entity[E]](entity: E, meta: EntityMeta[E]): Boolean = {
    import com.google.appengine.api.datastore.Query.CompositeFilterOperator._
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

  private[datastore] def lt[E <: Entity[E]](entity1: E, entity2: E, meta: EntityMeta[E]): Boolean
}

case class SortPredicate[T](name: String, direction: LLSortDirection, property: IndexedProperty[T]) extends Sort {

  private[datastore] def toLLSortDirection = new LLSortPredicate(name, direction)

  private[datastore] def lt[E <: Entity[E]](entity1: E, entity2: E, meta: EntityMeta[E]): Boolean = {
    val v1: T = property.fromStoreProperty(meta.toLLEntity(entity1).getProperty(name))
    val v2: T = property.fromStoreProperty(meta.toLLEntity(entity2).getProperty(name))

    property.compare(v1, v2) < 0
  }

}


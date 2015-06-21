package com.xhachi.gae4s.datastore

import com.google.appengine.api.datastore.Query.{CompositeFilter => LLCompositeFilter, CompositeFilterOperator, Filter => LLFilter, FilterOperator, FilterPredicate => LLFilterPredicate, SortDirection => LLSortDirection, SortPredicate => LLSortPredicate}
import com.google.appengine.api.datastore.{Query => LLQuery}

import scala.collection.JavaConversions._
import scala.language.experimental.macros
import scala.language.implicitConversions


object Query {
  def apply[E <: Entity[E]](implicit meta: EntityMeta[E]): Query[E] = Query(meta, None)
  def apply[E <: Entity[E]](ancestor: Key[E])(implicit meta: EntityMeta[E]): Query[E] = Query(meta, Some(ancestor))
}

case class Query[E <: Entity[E]] private[datastore](meta: EntityMeta[E],
                                                    ancestorOption: Option[Key[_]] = None,
                                                    filterOption: Option[Filter] = None,
                                                    sorts: Seq[Sort] = Nil,
                                                    offset: Option[Int] = None,
                                                    limit: Option[Int] = None) {

  def ancestor(ancestor: Key[_]): Query[E] = copy(ancestorOption = Some(ancestor))

  def filterByMeta(filter: EntityMeta[E] => Filter): Query[E] = copy(filterOption = Some(filter(meta)))

  def filter(filter: E => Boolean): Query[E] = macro EntityMacro.filter[E]

  def sortByMeta(sort: EntityMeta[E] => Sort): Query[E] = copy(sorts = sort(meta) :: Nil)

  def sort(sort: E => Any): Query[E] = macro EntityMacro.sort[E]

  def sortDesc(sort: E => Any): Query[E] = macro EntityMacro.sortDesc[E]

  def offset(o: Int): Query[E] = copy(offset = Some(o))

  def limit(l: Int): Query[E] = copy(limit = Some(l))

  private[datastore] def toLLQuery(keysOnly: Boolean): LLQuery = {
    val query = new LLQuery(meta.kind)
    if (keysOnly) query.setKeysOnly() else query.clearKeysOnly()
    if (ancestorOption.isDefined) query.setAncestor(ancestorOption.get.key)
    if (filterOption.isDefined) query.setFilter(filterOption.get.toLLFilter)
    sorts.foreach(s => query.addSort(s.name, s.direction))
    query
  }
}

trait Filter extends Serializable {

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

trait Sort extends Serializable {

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


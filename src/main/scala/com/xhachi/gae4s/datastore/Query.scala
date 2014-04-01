package com.xhachi.gae4s.datastore

import scala.collection.JavaConversions._
import com.google.appengine.api.datastore.Query.{Filter => LLFilter}
import com.google.appengine.api.datastore.Query.{FilterPredicate => LLFilterPredicate}
import com.google.appengine.api.datastore.Query.{SortDirection => LLSortDirection}
import com.google.appengine.api.datastore.Query.{SortPredicate => LLSortPredicate}
import com.google.appengine.api.datastore.Query.{CompositeFilter => LLCompositeFilter}
import com.google.appengine.api.datastore.Query.{CompositeFilterOperator, SortDirection, FilterOperator}
import com.google.appengine.api.datastore.{Query => LLQuery, Transaction}

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

  def count = datastore.count(this)

  def asSeq = datastore.asSeq(this)

  def asKeySeq = datastore.asKeySeq(this)

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
}

case class CompositeFilterPredicate(operator: CompositeFilterOperator, filters: Seq[Filter]) extends Filter {

  private[datastore] def toLLFilter = new LLCompositeFilter(operator, filters.map(_.toLLFilter))

}


trait Sort {

  private[datastore] def name: String

  private[datastore] def direction: LLSortDirection
}

case class SortPredicate(name: String, direction: SortDirection) extends Sort {

  private[datastore] def toLLSortDirection = new LLSortPredicate(name, direction)
}


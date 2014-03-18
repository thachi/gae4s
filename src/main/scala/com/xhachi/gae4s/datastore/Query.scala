package com.xhachi.gae4s.datastore


import scala.collection.JavaConversions._
import com.google.appengine.api.datastore.Query.{Filter => LLFilter}
import com.google.appengine.api.datastore.Query.{FilterPredicate => LLFilterPredicate}
import com.google.appengine.api.datastore.Query.{SortDirection => LLSortDirection}
import com.google.appengine.api.datastore.Query.{SortPredicate => LLSortPredicate}
import com.google.appengine.api.datastore.Query.{CompositeFilter => LLCompositeFilter}
import com.google.appengine.api.datastore.Query.{CompositeFilterOperator, SortDirection, FilterOperator}
import com.google.appengine.api.datastore.{Query => LLQuery}


case class Query[E <: Entity[E], M <: EntityMeta[E]] private[datastore](
                                                                         val datastore: Datastore,
                                                                         val meta: M,
                                                                         val filterOption: Option[Filter] = None,
                                                                         val sorts: Seq[Sort] = Nil
                                                                         ) {
  def filter(filters: (M => Filter)): Query[E, M] = {
    val f: Option[Filter] = Some(filters(meta))
    copy(filterOption = f)
  }

  def sort(sort: (M => Sort), sorts: (M => Sort)*): Query[E, M] = {
    val s: Seq[Sort] = sort(meta) :: sorts.map(_(meta)).toList
    copy(sorts = s)
  }

  def count = datastore.count(this)

  def asSeq = datastore.asSeq(this)

  def asKeySeq = datastore.asKeySeq(this)

  private[datastore] def toLLQuery(keysOnly: Boolean): LLQuery = {
    val query = new LLQuery(meta.kind)
    if (keysOnly) query.setKeysOnly() else query.clearKeysOnly()
    if (filterOption.isDefined) query.setFilter(filterOption.get.toLLFilter)
    sorts.foreach(s => query.addSort(s.name, s.direction))
    query
  }

}


trait Filter {

  private[datastore] def toLLFilter: LLFilter

  def &&(f: Filter): Filter = {
    CompositeFilter(CompositeFilterOperator.AND, this :: f :: Nil)
  }

  def ||(f: Filter): Filter = {
    CompositeFilter(CompositeFilterOperator.OR, this :: f :: Nil)
  }
}

case class FilterPredicate(name: String, operator: FilterOperator, value: Any) extends Filter {

  def iAmFilterPredicate = null

  private[datastore] def toLLFilter = (operator, value) match {
    case (FilterOperator.IN, value: Seq[_]) => new LLFilterPredicate(name, operator, seqAsJavaList(value))
    case _ => new LLFilterPredicate(name, operator, value)
  }
}

case class CompositeFilter(operator: CompositeFilterOperator, filters: Seq[Filter]) extends Filter {

  def iAmCompositeFilter = null

  private[datastore] def toLLFilter = new LLCompositeFilter(operator, filters.map(_.toLLFilter))
}


trait Sort {

  private[datastore] def name: String

  private[datastore] def direction: LLSortDirection

}

case class SortPredicate(name: String, direction: SortDirection) extends Sort {

  private[datastore] def toLLSortDirection = new LLSortPredicate(name, direction)
}


package com.xhachi.gae4s.datastore


import scala.collection.JavaConversions._
import com.google.appengine.api.datastore.Query.{Filter => LLFilter}
import com.google.appengine.api.datastore.Query.{FilterPredicate => LLFilterPredicate}
import com.google.appengine.api.datastore.Query.{SortDirection => LLSortDirection}
import com.google.appengine.api.datastore.Query.{SortPredicate => LLSortPredicate}
import com.google.appengine.api.datastore.Query.{CompositeFilter => LLCompositeFilter}
import com.google.appengine.api.datastore.Query.{CompositeFilterOperator, SortDirection, FilterOperator}


case class Query[E <: Entity[E], M <: EntityMeta[E]] private[datastore](
                                                                         val datastore: Datastore,
                                                                         val meta: M,
                                                                         val filterOption: Option[Filter] = None
                                                                         ) {
  def filter(filters: (M => Filter)): Query[E, M] = {
    val f: Option[Filter] = Some(filters(meta))
    copy(filterOption = f)
  }

  def count = datastore.count(this)

  def asSeq = datastore.asSeq(this)

  def asKeySeq = datastore.asKeySeq(this)
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

case class SortPredicate(name: String, operator: SortDirection) {

  private[datastore] def toLLSortDirection = new LLSortPredicate(name, operator)
}


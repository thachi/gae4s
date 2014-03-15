package com.xhachi.gae4s.datastore

import scala.reflect.runtime.universe._


import scala.collection.JavaConversions._
import com.google.appengine.api.datastore.Query.{Filter => LLFilter}
import com.google.appengine.api.datastore.Query.{FilterPredicate => LLFilterPredicate}
import com.google.appengine.api.datastore.Query.{SortDirection => LLSortDirection}
import com.google.appengine.api.datastore.Query.{SortPredicate => LLSortPredicate}
import com.google.appengine.api.datastore.Query.{CompositeFilter => LLCompositeFilter}
import com.google.appengine.api.datastore.Query.{CompositeFilterOperator, SortDirection, FilterOperator}


case class Query[E <: Entity[E] : TypeTag] private[datastore](
                                                               val kind: String,
                                                               val datastore: Datastore,
                                                               val keysOnly: Boolean = false
                                                               ) extends EntityConverter {


}


trait Filter {

  private[datastore] def toLLFilter: LLFilter
}

case class FilterPredicate(name: String, operator: FilterOperator, value: Any) extends Filter {

  def iAmFilterPredicate = null

  private[datastore] def toLLFilter = new LLFilterPredicate(name, operator, value)
}

case class CompositeFilter(operator: CompositeFilterOperator, filters: Seq[Filter]) extends Filter {

  def iAmCompositeFilter = null

  private[datastore] def toLLFilter = new LLCompositeFilter(operator, filters.map(_.toLLFilter))
}

case class SortPredicate(name: String, operator: SortDirection) {

  private[datastore] def toLLSortDirection = new LLSortPredicate(name, operator)
}


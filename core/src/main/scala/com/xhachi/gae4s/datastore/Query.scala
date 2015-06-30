package com.xhachi.gae4s.datastore


import scala.language.experimental.macros


object Query {
  def apply[E <: Entity[E]](implicit meta: EntityMeta[E]): Query[E] = Query(meta, None)

  def apply[E <: Entity[E]](ancestor: Key[E])(implicit meta: EntityMeta[E]): Query[E] = Query(meta, Some(ancestor))
}

case class Query[E <: Entity[E]](meta: EntityMeta[E],
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

}

object Filter {

  sealed trait CompositeOperator

  case object And extends CompositeOperator

  case object Or extends CompositeOperator

  sealed trait ComparativeOperator

  case object Equal extends ComparativeOperator

  case object NotEqual extends ComparativeOperator

  case object LessThan extends ComparativeOperator

  case object LessThanOrEqual extends ComparativeOperator

  case object GreaterThan extends ComparativeOperator

  case object GreaterThanOrEqual extends ComparativeOperator

  case object In extends ComparativeOperator

}

sealed trait Filter extends Serializable {

  private[datastore] def isMatch[E <: Entity[E]](entity: E, meta: EntityMeta[E]): Boolean

  def &&(f: Filter): Filter = {
    CompositeFilterPredicate(Filter.And, this :: f :: Nil)
  }

  def ||(f: Filter): Filter = {
    CompositeFilterPredicate(Filter.Or, this :: f :: Nil)
  }
}

case class FilterPredicate[T](property: IndexedProperty[T], operator: Filter.ComparativeOperator, values: Seq[T] = Nil) extends Filter {

  import com.xhachi.gae4s.datastore.Filter._

  if (operator != In && values.size != 1) throw new IllegalArgumentException("valid values")

  private[datastore] def isMatch[E <: Entity[E]](entity: E, meta: EntityMeta[E]): Boolean = {
    val v: T = property.fromStoreProperty(meta.toLLEntity(entity).getProperty(property.name))

    operator match {
      case Equal => property.compare(v, values.head) == 0
      case NotEqual => property.compare(v, values.head) != 0
      case LessThan => property.compare(v, values.head) < 0
      case LessThanOrEqual => property.compare(v, values.head) <= 0
      case GreaterThan => 0 < property.compare(v, values.head)
      case GreaterThanOrEqual => 0 <= property.compare(v, values.head)
      case In => values.contains(v)
    }
  }
}

case class CompositeFilterPredicate(operator: Filter.CompositeOperator, filters: Seq[Filter]) extends Filter {

  import com.xhachi.gae4s.datastore.Filter._

  private[datastore] def isMatch[E <: Entity[E]](entity: E, meta: EntityMeta[E]): Boolean = {
    operator match {
      case And => filters.map(f => f.isMatch(entity, meta)).filter(m => !m).isEmpty
      case Or => filters.map(f => f.isMatch(entity, meta)).filter(m => m).nonEmpty
    }
  }
}

object Sort {

  trait Direction

  object Direction {

    case object Ascending extends Direction

    case object Descending extends Direction

  }

}

sealed trait Sort extends Serializable {

  private[datastore] def name: String

  private[datastore] def direction: Sort.Direction

  private[datastore] def lt[E <: Entity[E]](entity1: E, entity2: E, meta: EntityMeta[E]): Boolean
}

case class SortPredicate[T](name: String, direction: Sort.Direction, property: IndexedProperty[T]) extends Sort {

  private[datastore] def lt[E <: Entity[E]](entity1: E, entity2: E, meta: EntityMeta[E]): Boolean = {
    val v1: T = property.fromStoreProperty(meta.toLLEntity(entity1).getProperty(name))
    val v2: T = property.fromStoreProperty(meta.toLLEntity(entity2).getProperty(name))

    property.compare(v1, v2) < 0
  }
}


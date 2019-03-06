package com.xhachi.gae4s.datastore


import scala.language.experimental.macros
import scala.language.implicitConversions


object Query {

  object Implicits {
    implicit def f(name: String): FilterMeta = FilterMeta(name)

    implicit def s(name: String): SortPredicate = SortPredicate(name, Sort.Direction.Ascending)
  }

  def apply(kind: String): Query = Query(kind, None)
}

case class Query(kind: String,
                 ancestorOption: Option[Key] = None,
                 filterOption: Option[Filter] = None,
                 sorts: Seq[Sort] = Nil,
                 offset: Option[Int] = None,
                 limit: Option[Int] = None) {


  def ancestor(ancestor: Key): Query = copy(ancestorOption = Some(ancestor))

  def ancestor(ancestor: Option[Key]): Query = copy(ancestorOption = ancestor)

  def filter(filter: Filter): Query = copy(filterOption = Some(filter))

  def sort(sorts: Sort*): Query = copy(sorts = sorts.toSeq)

  def offset(o: Int): Query = copy(offset = Some(o))

  def offset(o: Option[Int]): Query = copy(offset = o)

  def limit(l: Int): Query = copy(limit = Some(l))

  def limit(l: Option[Int]): Query = copy(limit = l)

}

class MetaFactory {
  def apply(name: String) = FilterMeta(name)
}

case class FilterMeta(name: String) {

  def unary_! : Filter = FilterPredicate[Boolean](name, Filter.Equal, false :: Nil)

  def ===[T](value: T): Filter = FilterPredicate[T](name, Filter.Equal, value :: Nil)

  def !==[T](value: T): Filter = FilterPredicate[T](name, Filter.NotEqual, value :: Nil)

  def >=[T](value: T): Filter = FilterPredicate[T](name, Filter.GreaterThanOrEqual, value :: Nil)

  def >[T](value: T): Filter = FilterPredicate[T](name, Filter.GreaterThan, value :: Nil)

  def <=[T](value: T): Filter = FilterPredicate[T](name, Filter.LessThanOrEqual, value :: Nil)

  def <[T](value: T): Filter = FilterPredicate[T](name, Filter.LessThan, value :: Nil)

  def in[T](values: T*): Filter = FilterPredicate[T](name, Filter.LessThan, values)

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

  def &&(f: Filter): Filter = {
    CompositeFilterPredicate(Filter.And, this :: f :: Nil)
  }

  def ||(f: Filter): Filter = {
    CompositeFilterPredicate(Filter.Or, this :: f :: Nil)
  }
}

case class FilterPredicate[T](name: String, operator: Filter.ComparativeOperator, values: Seq[T] = Nil) extends Filter {
  if (operator != Filter.In && values.size != 1) throw new IllegalArgumentException("valid values")
}

case class CompositeFilterPredicate(operator: Filter.CompositeOperator, filters: Seq[Filter]) extends Filter

object Sort {

  trait Direction

  object Direction {

    case object Ascending extends Direction

    case object Descending extends Direction

  }

}

sealed trait Sort extends Serializable {

  import Sort._

  private[datastore] def name: String

  private[datastore] def direction: Direction

  def asc = SortPredicate(name, Direction.Ascending)

  def desc = SortPredicate(name, Direction.Descending)
}

case class SortPredicate(name: String, direction: Sort.Direction) extends Sort


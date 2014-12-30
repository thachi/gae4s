package com.xhachi.gae4s.datastore

import com.google.appengine.api.datastore.Query.{CompositeFilterOperator, FilterOperator, CompositeFilter => LLCompositeFilter, Filter => LLFilter, FilterPredicate => LLFilterPredicate, SortDirection => LLSortDirection, SortPredicate => LLSortPredicate}
import com.google.appengine.api.datastore.{Transaction, Query => LLQuery}

import scala.collection.JavaConversions._
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

case class Query[E <: Entity[E], M <: EntityMeta[E]] private[datastore](
                                                                         datastore: DatastoreQueryMethods,
                                                                         meta: M,
                                                                         tx: Option[Transaction],
                                                                         ancestorOption: Option[Key[_]] = None,
                                                                         filterOption: Option[Filter] = None,
                                                                         sorts: Seq[Sort] = Nil,
                                                                         offset: Option[Int] = None,
                                                                         limit: Option[Int] = None
                                                                         ) {

  def ancestor(ancestor: Key[_]): Query[E, M] = copy(ancestorOption = Some(ancestor))

  def filter(filters: (M => Filter)): Query[E, M] = copy(filterOption = Some(filters(meta)))

  def sort(sort: (M => Sort), sorts: (M => Sort)*): Query[E, M] = copy(sorts = sort(meta) :: sorts.map(_(meta)).toList)

  def offset(o: Int): Query[E, M] = copy(offset = Some(o))

  def limit(l: Int): Query[E, M] = copy(limit = Some(l))

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

//  def mFilter(filter: E => Boolean): Query[E, M] = {
//    val f = this.mF(filter)
//    copy(
//      filterOption = Some(f)
//    )
//  }
  def mF(filter: E => Boolean): Query[E, M] = macro QueryMacro.filter[E, M]

  def mSortBy(sort: E => Any): Query[E, M] = macro QueryMacro.sortBy[E, M]

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


object QueryMacro {
  def filter[E <: Entity[E] : c.WeakTypeTag,M <: EntityMeta[E] : c.WeakTypeTag](c: Context)(filter: c.Expr[E => Boolean]): c.Expr[Query[E, M]] = {
    import c.universe._


    c.Expr[Query[E, M]](q"null")
  }

  def sortBy[E <: Entity[E] : c.WeakTypeTag,M <: EntityMeta[E] : c.WeakTypeTag](c: Context)(sort: c.Expr[E => Any]): c.Expr[Query[E, M]] = {
    import c.universe._


    def printChildren(tree: Tree, depth: Int = 0): Unit = {
      tree match {
        case t: Tree if t.isType =>
          println(("\t" * depth) + t.symbol + " (type)")
        case t: Apply =>
          println(("\t" * depth) + t.symbol + " (Apply)")
        case t: Select =>
          println(("\t" * depth) + t.symbol + " (Select)" + t.symbol.name)
        case t: Ident =>
          println(("\t" * depth) + t.symbol + " (Ident)" + t.symbol.asTerm.typeSignature)
        case t: Tree =>
          println(("\t" * depth) + t.symbol + " (" + t.getClass + ")")
      }
      tree.children.foreach(child => printChildren(child, depth + 1))
    }


          println("--- QueryMacro ---")

    val prefix = c.prefix.tree
    println(sort)

    printChildren(sort.tree, 0)

    sort.tree match {
      case t => t
    }
    val ow = c.macroApplication.symbol.owner
    c.Expr[Query[E, M]](q"$prefix")
  }

}
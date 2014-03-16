package com.xhachi.gae4s.datastore

import com.google.appengine.api.datastore.{Key => LLKey, Entity => LLEntity, Query => LLQuery}
import com.google.appengine.api.datastore.Query.{FilterPredicate => LLFilterPredicate, SortDirection => LLSortDirection}
import com.google.appengine.api.datastore.Query.{CompositeFilter => LLCompositeFilter}


trait KeyConverter {

  implicit private[datastore] def toLLKey[E <: Entity[E]](key: Key[E]): LLKey = key.key

  implicit private[datastore] def toKey[E <: Entity[E]](key: LLKey): Key[E] = Key[E](key)

}
/*
trait EntityConverter {
  import scala.reflect.runtime.universe
  import scala.reflect.runtime.universe._


  private[datastore] def toLLEntity[E <: Entity[E] : TypeTag](entity: E): LLEntity = {

    val mirror = universe.typeTag[E].mirror

    val lle = entity.keyOption match {
      case Some(k) => new LLEntity(k.key)
      case None => new LLEntity("kind")
    }

    lle
  }

  private[datastore] def toEntity[E <: Entity[E] : TypeTag](entity: LLEntity): E = {
    val mirror = universe.typeTag[E].mirror
    val clazz = mirror.runtimeClass(typeOf[E])
    def getType[U: TypeTag](obj: Class[_]): Type = typeOf[U]

    val t: Type = getType[E](clazz.getClass)
    val classSymbol = t.typeSymbol.asClass
    val classMirror = mirror.reflectClass(classSymbol)
    val constructorMethod = t.member(universe.nme.CONSTRUCTOR).asMethod
    val constructorMirror = classMirror.reflectConstructor(constructorMethod)

    println(constructorMirror)

    val args = for (params <- constructorMethod.paramss; param <- params) yield {
      val name: String = param.name.decoded
      println(param.name)

      if (entity.hasProperty(name)) {
        entity.getProperty(name)
      }
      else {
        null
      }
    }

    println(args.size)
    println(args)

    val e = constructorMirror.apply(args: _*).asInstanceOf[E]
    e
  }


}
*/

trait QueryConverter {

  def toLLQuery[E <: Entity[E]](q: Query[E]): LLQuery = {
    val llq = new LLQuery("kind")
    if (q.keysOnly) llq.setKeysOnly() else llq.clearKeysOnly()
    llq
  }

}


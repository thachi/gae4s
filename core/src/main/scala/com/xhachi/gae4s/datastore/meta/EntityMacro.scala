package com.xhachi.gae4s.datastore.meta

import com.xhachi.gae4s.datastore.{Entity, EntityMeta}

import scala.reflect.macros.blackbox.Context


object EntityMacro {


  def createMeta[E <: Entity[E] : c.WeakTypeTag](c: Context): c.Expr[EntityMeta[E]] = {
    import c.universe._

    val entityType = c.weakTypeOf[E]

    val propertyMembers = entityType.decls.filter { m =>
      m.annotations.size != 0
    }

    val propertyMembers2 = entityType.decls.filter(_.isMethod).filter(_.asMethod.isConstructor).map { m =>
      m.asMethod.paramLists.map(_.map(_.annotations))
    }

    println("--- EntityMacro ---")
    println(entityType.decls)
    println("--- propertyMembers ---")
    println(propertyMembers)
    println("--- propertyMembers2 ---")
    println(propertyMembers2)


    val properties = entityType
      .decls
      .filter(_.name match {
      case t: TermName if t.encodedName.toString == "key" =>
        println("name term false: " + t + ":" + t.getClass)
        false
      case t: TermName =>
        println("name term true: " + t + ":" + t.encodedName.toString)
        true
      case n =>
        println("name true: " + n + ":" + n.getClass)
        true
    })
      .map {
      case m: TermSymbol if m.isVar =>
        val memberType = m.typeSignature
        val TypeName(typeName) = memberType.typeSymbol.name
        val TermName(termName) = m.name
        val propertyName = TermName(termName.trim)
        val propertyType = TypeName(typeName + "Property")

        println(s"property: $termName: $memberType")

        if (memberType =:= typeOf[Option[String]]) {
//          Some( q""" val $propertyName = new OptionProperty(new StringProperty(${termName.trim}))""")

          Some(propertyName -> q"""new com.xhachi.gae4s.datastore.OptionProperty(new com.xhachi.gae4s.datastore.StringProperty(${termName.trim}))""")

        }
        else if (memberType =:= typeOf[String]) {
//          Some( q""" val $propertyName = new StringProperty(${termName.trim})""")
          Some(propertyName -> q"""new com.xhachi.gae4s.datastore.StringProperty(${termName.trim})""")
        }
        else if (memberType =:= typeOf[Int]) {
//          Some( q""" val $propertyName = new IntProperty(${termName.trim})""")
          Some(propertyName -> q"""new com.xhachi.gae4s.datastore.IntProperty(${termName.trim})""")
        }
        else {
//          Some( q""" val $propertyName = $typeName""")
          None
        }

      case m =>
        println("                                                     not property: " + m)
        None

    }.toSeq.filter(_.isDefined).map(_.get)


    val fields = properties.map{
      case (n, v) => q"""val `$n` = $v"""
    }

    val toEntities = properties.map{
      case (n, v) => q"""to.$n = $n.getValueFromLLEntity(from)"""
    }
    val toLLEntities = properties.map{
      case (n, v) => q"""$n.setValueToLLEntity(to)(from.$n)"""
    }

    val names = properties.map(_._1)

    println(fields.mkString("\n"))


    val tree = q"""
new com.xhachi.gae4s.datastore.EntityMeta[$entityType] {


  ..$fields

  override val properties = Seq(
  ..$names
  )

  def createEntity(key: Key[$entityType]) = new $entityType(key)

  override def toEntity(from: com.google.appengine.api.datastore.Entity): $entityType = {
    from match {
      case _: com.google.appengine.api.datastore.Entity =>
        val to = createEntity(createKey(from.getKey))
        ..$toEntities
        to
      case _ => null.asInstanceOf[$entityType]
    }
  }

  override def toLLEntity(from: $entityType): com.google.appengine.api.datastore.Entity = {
    from match {
      case _: $entityType =>
        val to = new com.google.appengine.api.datastore.Entity(from.key.key)
        ..$toLLEntities
        to
      case _ => null.asInstanceOf[com.google.appengine.api.datastore.Entity]
    }
  }
}

       """
/*



 */


    println("--- Created EntityMeta ---")
    println(tree)

    c.Expr[EntityMeta[E]](
      tree)
  }
/*
  def toProperty[E <: Entity[E] : c.WeakTypeTag](c: Context)(m: c.universe.Symbol): Option[(c.TermName, c.Tree)] = {
    import c.universe._

    val memberType = m.typeSignature
    val TypeName(typeName) = memberType.typeSymbol.name
    val TermName(termName) = m.name
    val propertyName = TermName(termName.trim)
    val propertyType = TypeName(typeName + "Property")

    println(s"property: $termName: $memberType")

    if (memberType =:= typeOf[Option[String]]) {
      //          Some( q""" val $propertyName = new OptionProperty(new StringProperty(${termName.trim}))""")

      Some(propertyName -> q"""new com.xhachi.gae4s.datastore.OptionProperty(new com.xhachi.gae4s.datastore.StringProperty(${termName.trim}))""")

    }
    else if (memberType =:= typeOf[String]) {
      //          Some( q""" val $propertyName = new StringProperty(${termName.trim})""")
      Some(propertyName -> q"""new com.xhachi.gae4s.datastore.StringProperty(${termName.trim})""")
    }
    else if (memberType =:= typeOf[Int]) {
      //          Some( q""" val $propertyName = new IntProperty(${termName.trim})""")
      Some(propertyName -> q"""new com.xhachi.gae4s.datastore.IntProperty(${termName.trim})""")
    }
    else {
      //          Some( q""" val $propertyName = $typeName""")
      None
    }

  }
 */

}



package com.xhachi.gae4s.datastore

import java.util.Date

import com.xhachi.gae4s.datastore.annotations._

import scala.reflect.macros.blackbox.{Context => BContext}


object EntityMacro {

  def createMeta[E <: Entity[E] : c.WeakTypeTag](c: BContext): c.Expr[EntityMeta[E]] = {
    import c.universe._

    val entityType = weakTypeOf[E]
    val metaName = TypeName(entityType.typeSymbol.name.decodedName + "Meta")
    val tree = toEntityMetaTree(c)(entityType, metaName)
    c.Expr[EntityMeta[E]]( q"""$tree; new $metaName""")
  }

  def filter[E <: Entity[E] : c.WeakTypeTag](c: BContext)(filter: c.Expr[E => Boolean]): c.Expr[Query[E]] = {
    import c.universe._
    val h = new Helper[c.type](c)

    val query = c.prefix.tree
    val entityType = c.weakTypeOf[E]
    val metaName = TypeName(entityType.typeSymbol.name.decodedName + "MetaForFilter")
    val name = h.findValueTermName(c)(filter.tree)

    name.map { n =>
      val replacing = h.replaceAllIdent(c)(filter.tree, n, TermName("meta"))
      val replacing1 = h.replaceAllSelectTermName(c)(replacing, TermName("$eq$eq"), TermName("isEqual"))
      val replacing2 = h.replaceAllSelectTermName(c)(replacing1, TermName("$greater$eq"), TermName("isGreaterThanOrEqual"))
      val replacing3 = h.replaceAllSelectTermName(c)(replacing2, TermName("$less$eq"), TermName("isLessThanOrEqual"))
      val replacing4 = h.replaceAllSelectTermName(c)(replacing3, TermName("$greater"), TermName("isGreaterThan"))
      val replacing5 = h.replaceAllSelectTermName(c)(replacing4, TermName("$less"), TermName("isLessThan"))
      val replacing6 = h.replaceAllSelectTermName(c)(replacing5, TermName("$bang$eq"), TermName("isNotEqual"))
      val replaced = h.findTopFunctionBody(c)(replacing6).map(h.replaceAll(c)(_))

      replaced.map {
        r =>
          val meta = toEntityMetaTree(c)(entityType, metaName)
          val t = c.Expr(Function(Nil, r.asInstanceOf[Tree]))
          val tree =
            q"""
$meta;
val meta = new $metaName
val f: com.xhachi.gae4s.datastore.Filter = $t()
$query.copy(filterOption = Some(f))
"""
          c.Expr[Query[E]](tree)
      }
    }.flatten.getOrElse {
      c.Expr[Query[E]](q"null")
    }
  }

  def sort[E <: Entity[E] : c.WeakTypeTag](c: BContext)(sort: c.Expr[E => Any]): c.Expr[Query[E]] = _sort[E](c)(sort, asc = true).asInstanceOf[c.Expr[Query[E]]]

  def sortDesc[E <: Entity[E] : c.WeakTypeTag](c: BContext)(sort: c.Expr[E => Any]): c.Expr[Query[E]] = _sort[E](c)(sort, asc = false).asInstanceOf[c.Expr[Query[E]]]

  private def _sort[E <: Entity[E] : c.WeakTypeTag](c: BContext)(sort: c.Expr[E => Any], asc: Boolean): c.Expr[Query[E]] = {
    import c.universe._
    val h = new Helper[c.type](c)
    //    println(s"---- SORT ----\n\n$sort\n\n--------------")

    val query = c.prefix.tree
    val entityType = c.weakTypeOf[E]
    val metaName = TypeName(entityType.typeSymbol.name.decodedName + "MetaForSort")

    val names = h.findValueTermName(c)(sort.tree)

    names.map {
      valName =>
        object sortTraverser extends Traverser {
          var sort: Option[TermName] = None

          override def traverse(tree: Tree): Unit = tree match {
            case Select(Ident(vn), s) if vn == valName.decodedName =>
              sort = Some(s.asInstanceOf[TermName])
              super.traverse(tree)
            case _ =>
              super.traverse(tree)
          }
        }
        sortTraverser.traverse(sort.tree)
        sortTraverser.sort.map {
          s =>
            val meta = toEntityMetaTree(c)(entityType, metaName)
            val tree = if (asc) {
              q"""
$meta;
val meta = new $metaName;
$query.copy(sorts = Seq(meta.$s.asc))
"""
            } else {
              q"""
$meta;
val meta = new $metaName;
$query.copy(sorts = Seq(meta.$s.desc))
"""
            }
            c.Expr[Query[E]](tree)
        }
    }.flatten.getOrElse {
      //      println("--- val not found ---")
      // TODO: Notify error
      c.Expr[Query[E]](q"$query")
    }
  }

  def toEntityMetaTree[E <: Entity[E] : c.WeakTypeTag](c: BContext)(entityType: c.Type, metaName: c.TypeName): c.Tree = {
    import c.universe._
    val h = new Helper[c.type](c)

    val entityType = c.weakTypeOf[E]

    case class PropertyInfo(name: TermName,
                            tpe: Type,
                            bases: Seq[String],
                            serializable: Boolean,
                            json: Boolean,
                            transient: Boolean,
                            indexed: Boolean,
                            version: Boolean,
                            creationDate: Boolean,
                            modificationDate: Boolean,
                            readonly: Boolean,
                            listener: Seq[Type]) {


      val stringName: String = {
        val TermName(n) = name
        n
      }
      val isOption: Boolean = tpe.typeSymbol.fullName == "scala.Option"
      val isSeq: Boolean = tpe.typeSymbol.fullName == "scala.collection.Seq"
      val isContainer: Boolean = isOption || isSeq

      val storeType = if ((isOption || (isSeq && !json)) && tpe.typeArgs.nonEmpty) tpe.typeArgs.head else tpe
      val isKey = storeType.typeSymbol.fullName == "com.xhachi.gae4s.datastore.Key"
      val keyType: Type = storeType.typeArgs.headOption.getOrElse(NoType)
      val isScalaEnum = storeType.typeSymbol.fullName == "scala.Enumeration.Value"
      val isJavaEnum = 1 < storeType.baseClasses.size && storeType.baseClasses.drop(1).head.fullName == "java.lang.Enum"

      val scalaEnumName = if (isScalaEnum) storeType.toString.split('.').dropRight(1).mkString(".") else null
      val scalaEnumModule: ModuleSymbol = if (isScalaEnum) c.mirror.staticModule(scalaEnumName) else null

      override def toString = {
        s"""PropertyInfo(
          name: $name,
          stringName: $stringName,
          tpe: $tpe,
          storeType: $storeType,
          bases: $bases,
          serializable: $serializable,
          json: $json,
          transient: $transient,
          indexed: $indexed,
          javaEnum: $isJavaEnum,
          scalaEnum: $isScalaEnum,
          scalaEnumName: $scalaEnumName,
          version: $version,
          creationDate: $creationDate,
          modificationDate: $modificationDate,
          readonly: $readonly,
          listener: $listener,
          isOption: $isOption
          isSeq: $isSeq
          )
        """
      }
    }

    def toPropertyInfo(name: TermName): PropertyInfo = {
      val member0 = entityType.member(name)
      val member1 = entityType.member(TermName(name + "_$eq"))
      val member2 = entityType.member(TermName(name + " "))
      assert(member0.isMethod)

      val hasTransient = (member2.isTerm && member2.asTerm.isVar && h.hasAnnotation(member2, typeOf[transient])) || h.hasAnnotation(member0, typeOf[transient])
      val hasJson = (member2.isTerm && member2.asTerm.isVar && h.hasAnnotation(member2, typeOf[json])) || h.hasAnnotation(member0, typeOf[json])
      val hasSerialize = (member2.isTerm && member2.asTerm.isVar && h.hasAnnotation(member2, typeOf[serialize])) || h.hasAnnotation(member0, typeOf[serialize])
      val hasIndexed = (member2.isTerm && member2.asTerm.isVar && h.hasAnnotation(member2, typeOf[indexed])) || h.hasAnnotation(member0, typeOf[indexed])
      val hasVersion = member2.isTerm && member2.asTerm.isVar && member2.annotations.exists(_.tree.tpe =:= typeOf[version])
      val hasCreationDate = member2.isTerm && member2.asTerm.isVar && member2.annotations.exists(_.tree.tpe =:= typeOf[creationDate])
      val hasModificationDate = member2.isTerm && member2.asTerm.isVar && member2.annotations.exists(_.tree.tpe =:= typeOf[modificationDate])

      val existsVersionEntityAnnotation = findAnnotationValue("version").contains(name.toString)
      val existsCreationDateEntityAnnotation = findAnnotationValue("creationDate").contains(name.toString)
      val existsModificationDateEntityAnnotation = findAnnotationValue("modificationDate").contains(name.toString)

      val tpe = member0.asMethod.returnType
      val p = PropertyInfo(
        name,
        tpe,
        tpe.baseClasses.map(_.asType.fullName).toSeq,
        serializable = hasSerialize && tpe <:< typeOf[Serializable],
        json = hasJson,
        transient = hasTransient,
        indexed = hasIndexed,
        version = existsVersionEntityAnnotation || hasVersion,
        creationDate = existsCreationDateEntityAnnotation || hasCreationDate,
        modificationDate = existsModificationDateEntityAnnotation || hasModificationDate,
        readonly = !member1.isMethod,
        listener = Nil
      )
//      println("PropiertyInfo: " + p)
      p
    }

    def isMemberOfEntity(member: c.Symbol): Boolean = {
      member.owner.annotations.exists(_.tree.tpe == typeOf[entity]) || member.owner.asType.typeSignature.baseClasses.tail.exists(_.fullName == "com.xhachi.gae4s.datastore.Entity")
    }

    def findAnnotationValue(name: String): Seq[String] = {
      val symbols = entityType.typeSymbol :: entityType.baseClasses
      val annotations = symbols.map(_.annotations.filter(_.tree.tpe =:= typeOf[entity])).flatten

      //      println(s"Annotations($name)\n" + symbols.map(s => s"${s.name} : ${s.annotations.filter(_.tree.tpe =:= typeOf[entity])}").mkString("\n"))
      annotations.map(_.tree) collect {
        case Apply(a, List(Literal(Constant(v: String)), _, _)) if name == "version" => v
        case Apply(a, List(_, Literal(Constant(v: String)), _)) if name == "creationDate" => v
        case Apply(a, List(_, _, Literal(Constant(v: String)))) if name == "modificationDate" => v
      }
    }

    // TODO: Listenerの仕組みはtraitのフィールドのアノテーションを取得できるようになってから
    //  def getListener(member: c.Symbol): Option[] = {
    //    import c.universe._
    //    val tail = member.annotations.find(_.tree.tpe == typeOf[listener]).map(_.tree.children.tail)
    //    null
    //  }

    def toProperty(info: PropertyInfo): Option[(c.TermName, c.Tree)] = {
      import c.universe._
      import com.google.appengine.api.blobstore._
      import com.google.appengine.api.datastore._
      import com.google.appengine.api.users._

      if (!info.transient) {

        def isValueType(memberType: Type): Boolean = Seq(
          //      typeOf[String],
          //      typeOf[Short], typeOf[Int], typeOf[Integer], typeOf[Long], typeOf[Float], typeOf[Double],
          typeOf[BigInt], typeOf[BigDecimal],
          typeOf[Boolean],
          typeOf[Date],
          //      typeOf[Key],
          typeOf[BlobKey],
          typeOf[Text], typeOf[ShortBlob], typeOf[Blob],
          typeOf[GeoPt], typeOf[PostalAddress], typeOf[PhoneNumber],
          typeOf[Email], typeOf[User],
          typeOf[IMHandle], typeOf[Link], typeOf[Category], typeOf[Rating],
          typeOf[EmbeddedEntity]
        ).exists(_ =:= memberType)

        val propertyTree = if (info.version) {
          q"""new com.xhachi.gae4s.datastore.VersionProperty(${info.stringName})"""
        } else if (info.creationDate) {
          q"""new com.xhachi.gae4s.datastore.CreationDateProperty(${info.stringName})"""
        } else if (info.modificationDate) {
          q"""new com.xhachi.gae4s.datastore.ModificationDateProperty(${info.stringName})"""
        }
        else {

          case class TypeDesc(tpe: Symbol, typeArg: Option[Type] = None)

          def toTypeDesc(typeName: String, typeArg: Option[Type] = None) = TypeDesc(c.mirror.staticClass(typeName), typeArg)

          case class PropertyDesc(base: TypeDesc, arg: Tree, withTrait: Option[TypeDesc] = None, body: Seq[Tree] = Nil) {

            def treeAsInstance = (base.typeArg, withTrait) match {
              case (None, None) =>
                q"""new ${base.tpe}($arg) {..$body}"""
              case (None, Some(TypeDesc(t, None))) =>
                q"""new ${base.tpe}($arg) with $t {..$body}"""
              case (None, Some(TypeDesc(t, Some(traitTypeArg)))) =>
                q"""new ${base.tpe}($arg) with $t[$traitTypeArg] {..$body}"""
              case (Some(typeArg), None) =>
                q"""new ${base.tpe}[$typeArg]($arg) {..$body}"""
              case (Some(typeArg), Some(TypeDesc(t, None))) =>
                q"""new ${base.tpe}[$typeArg]($arg) with $t {..$body}"""
              case (Some(typeArg), Some(TypeDesc(t, Some(traitTypeArg)))) =>
                q"""new ${base.tpe}[$typeArg]($arg) with $t[$traitTypeArg] {..$body}"""
            }
          }

          def getPropertyDesc(t: Type): PropertyDesc = if (info.json) {
            PropertyDesc(toTypeDesc("com.xhachi.gae4s.datastore.JsonProperty", Some(info.storeType)), q"""${info.stringName}""")
          } else if (info.serializable) {
            PropertyDesc(toTypeDesc("com.xhachi.gae4s.datastore.SerializableProperty", Some(info.storeType)), q"""${info.stringName}""")
          } else if (isValueType(t)) {
            val propertyTypeName = TypeName(t.typeSymbol.asType.name.toTypeName + "Property")
            PropertyDesc(toTypeDesc(s"com.xhachi.gae4s.datastore.$propertyTypeName"), q"""${info.stringName}""")
          } else if (t =:= typeOf[String]) {
            PropertyDesc(toTypeDesc("com.xhachi.gae4s.datastore.StringProperty"), q"""${info.stringName}""")
          } else if (info.storeType =:= typeOf[Array[Byte]]) {
            PropertyDesc(toTypeDesc("com.xhachi.gae4s.datastore.ByteArrayProperty"), q"""${info.stringName}""")
          } else if (t =:= typeOf[Double]) {
            PropertyDesc(toTypeDesc("com.xhachi.gae4s.datastore.DoubleProperty"), q"""${info.stringName}""")
          } else if (t =:= typeOf[Int]) {
            PropertyDesc(toTypeDesc("com.xhachi.gae4s.datastore.IntProperty"), q"""${info.stringName}""")
          } else if (t =:= typeOf[Long]) {
            PropertyDesc(toTypeDesc("com.xhachi.gae4s.datastore.LongProperty"), q"""${info.stringName}""")
          } else if (info.isKey) {
            PropertyDesc(toTypeDesc("com.xhachi.gae4s.datastore.KeyProperty", Some(info.keyType)), q"""${info.stringName}""")
          } else if (info.isJavaEnum) {
            PropertyDesc(toTypeDesc("com.xhachi.gae4s.datastore.EnumProperty", Some(info.storeType)), q"""${info.stringName}""")
          } else if (info.isScalaEnum) {
            val enum = c.mirror.staticModule(info.scalaEnumName)
            PropertyDesc(toTypeDesc("com.xhachi.gae4s.datastore.StringStoreProperty", Some(info.storeType)),
              q"""${info.stringName}""",
              body = Seq(
                q"override def fromString(value: String): $enum.Value = $enum.withName(value)",
                q"override def toString(value: $enum.Value): String = value.toString"
              ))
          } else {
            c.abort(c.enclosingPosition, s"${info.name} as ${info.storeType} cannot be property\n\n" + info)
          }

          val p0 = getPropertyDesc(info.storeType)
          val p1 = if (info.isOption) {
            PropertyDesc(toTypeDesc("com.xhachi.gae4s.datastore.OptionProperty", Some(info.storeType)), p0.treeAsInstance)
          } else if (info.isSeq && !info.json) {
            PropertyDesc(toTypeDesc("com.xhachi.gae4s.datastore.SeqProperty", Some(info.storeType)), p0.treeAsInstance)
          } else {
            p0
          }

          val p2 = if (info.indexed) {
            p1.copy(withTrait = Some(toTypeDesc("com.xhachi.gae4s.datastore.IndexedProperty", Some(info.tpe))))
          } else {
            p1
          }

          p2.treeAsInstance
        }
        Some(info.name -> propertyTree)
      } else {
        None
      }
    }

    val propertyInfos = entityType.members
      .filter(m => m.isMethod && m.asMethod.paramLists.isEmpty)
      .filter(_.name.encodedName.toString != "key")
      .filter(isMemberOfEntity)
      .map(m => toPropertyInfo(m.name.toTermName))

    val readableProperties = propertyInfos.filterNot(_.version).filterNot(_.readonly)
      .map(i => toProperty(i))
      .toSeq
      .filter(_.isDefined).map(_.get)

    val readonlyProperties = propertyInfos.filterNot(_.version).filter(_.readonly)
      .map(i => toProperty(i))
      .toSeq
      .filter(_.isDefined).map(_.get)

    val versionProperty = propertyInfos.filter(_.version)
      .map(i => toProperty(i))
      .toSeq
      .filter(_.isDefined).map(_.get)
      .headOption

    val properties = readableProperties ++ readonlyProperties ++ versionProperty.toSeq

    val fields = properties.map {
      case (n, v) => q"""val $n = $v"""
    }

    val toEntities = (readableProperties ++ versionProperty.toSeq).map {
      case (n, v) => q"""to.$n = $n.getValueFromLLEntity(from)"""
    }
    val toLLEntities = properties.map {
      case (n, v) => q"""$n.setValueToLLEntity(to)(from.$n)"""
    }

    val names = properties.map(_._1)
    val versionName = versionProperty.map(_._1)

    val tree = q"""
class $metaName extends com.xhachi.gae4s.datastore.EntityMeta[$entityType] {

  ..$fields

  override val properties = Seq(
  ..$names
  )

  override val versionProperty: Option[com.xhachi.gae4s.datastore.VersionProperty] = $versionName

  def createEntity(key: com.xhachi.gae4s.datastore.Key[$entityType]) = new $entityType(key)

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
//    println(tree)
    tree
  }

}


class Helper[C <: BContext](val c: C) {


  def findValueTermName[E <: Entity[E] : c.WeakTypeTag](c: BContext)(tree: c.Tree): Option[c.TermName] = {
    import c.universe._

    var name: Option[TermName] = None

    new Traverser {
      override def traverse(tree: Tree): Unit = tree match {
        case ValDef(_, n, _, _) =>
          name = Some(n)
          super.traverse(tree)
        case _ =>
          super.traverse(tree)
      }
    }.traverse(tree)

    name
  }

  /**
   * デバッグのための出力を行います。
   */
  def printDebug(c: BContext)(tree: c.Tree): Unit = {
    import c.universe._

    var depth = 0
    def p(s: String) = println("\t" * depth + s)

    new Traverser {
      override def traverse(tree: Tree): Unit = {
        p("- " + tree + " ... " + tree.getClass)
        tree match {
          case t: Function =>
          case _ =>
        }
        depth += 1
        super.traverse(tree)
        depth -= 1
      }
    }.traverse(tree)
  }

  def findTopFunctionBody(c: BContext)(tree: c.Tree): Option[c.Tree] = {
    import c.universe._

    var body: Option[Tree] = None

    new Traverser {
      override def traverse(tree: Tree): Unit = tree match {
        case Function(_, b) =>
          body = Some(b)
        //super.traverse(tree)
        case _ =>
          super.traverse(tree)
      }
    }.traverse(tree)

    body
  }

  def replaceAll(c: BContext)(tree: c.Tree): c.Tree = {
    import c.universe._

    new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Function(vparams, body) =>
          super.transform(Function(vparams, body))
        case Select(qualifier, name) =>
          super.transform(Select(qualifier, name))
        case Apply(fun, args) =>
          super.transform(Apply(fun, args))
        case Ident(name) =>
          super.transform(Ident(name))
        case Literal(value) =>
          super.transform(Literal(value))
        case _ =>
          super.transform(tree)
      }
    }.transform(tree)
  }

  def replaceAllIdent(c: BContext)(tree: c.Tree, from: c.Name, to: c.Name): c.Tree = {
    import c.universe._

    new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Ident(vn) if vn == from =>
          super.transform(Ident(to))
        case _ =>
          super.transform(tree)
      }
    }.transform(tree)
  }

  def replaceAllSelectTermName(c: BContext)(tree: c.Tree, from: c.Name, to: c.Name): c.Tree = {
    import c.universe._

    new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Select(t, n) if n == from =>
          super.transform(Select(t, to))
        case _ =>
          super.transform(tree)
      }
    }.transform(tree)
  }

  def replaceAllValType(c: BContext)(tree: c.Tree, from: c.Type, to: c.Type): c.Tree = {
    import c.universe._

    new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case ValDef(m, n, t, r) if t.tpe =:= from =>
          super.transform(ValDef(m, n, TypeTree(to), r))
        case _ =>
          super.transform(tree)
      }
    }.transform(tree)
  }

  def hasAnnotation(member: c.Symbol, tpe: c.Type): Boolean = {
    member.annotations.exists(_.tree.tpe =:= tpe) || hasAnnotationInConstructorParam(member, tpe)
  }

  def hasAnnotationInConstructorParam(member: c.Symbol, tpe: c.Type): Boolean = {
    import c.universe._

    member.owner match {
      case o: TypeSymbol if o.typeSignature.member(termNames.CONSTRUCTOR).isMethod =>
        o.typeSignature.member(termNames.CONSTRUCTOR).asMethod
          .paramLists.flatMap(_.filter(_.name == member.name))
          .exists(_.annotations.exists(_.tree.tpe =:= tpe))
      case _ => false
    }
  }

}
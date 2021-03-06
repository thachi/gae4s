package com.xhachi.gae4s.datastore

import java.util.Date

import com.xhachi.gae4s.datastore.meta._

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
      c.abort(c.enclosingPosition, "Unsupported format for filter. ")
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
      c.abort(c.enclosingPosition, "Unsupported format for sort. " + sort)
    }
  }

  def toEntityMetaTree[E <: Entity[E] : c.WeakTypeTag](c: BContext)(entityType: c.Type, metaName: c.TypeName): c.Tree = {
    import c.universe._
    val h = new Helper[c.type](c)

    val entityType = c.weakTypeOf[E]
    val ancestorType = entityType.typeSymbol.typeSignature.baseType(typeOf[Ancestor[_]].typeSymbol) match {
      case NoType => None
      case a => a.typeArgs.headOption
    }

    case class PropertyInfo(name: TermName,
                            tpe: Type,
                            order: Int,
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
          order: $order,
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

    def toPropertyInfo(name0: TermName): Option[PropertyInfo] = {
      val name1 = TermName(name0 + "_$eq")
      val name2 = TermName(name0 + " ")
      val member0 = entityType.member(name0)
      val member1 = entityType.member(name1)
      val member2 = entityType.member(name2)
      assert(member0.isMethod)

      h.getAnnotation(entityType, Seq(name0, name1, name2), typeOf[property]) match {
        case Nil if !isMemberOfEntity(Seq(member0, member2, member2)) => None
        case annotations =>


          def getAnnotationValueOrElse[T](name: String, default: T): T = {
            val value = annotations.flatMap { a =>
              val param = a.tree.children.tail
              param.flatMap {
                case AssignOrNamedArg(Ident(TermName(n)), v) if n == name => Some(c.eval(c.Expr[T](v)))
                case _ => None
              }
            }
            value.headOption.getOrElse(default)
          }

          val hasTransient = h.getAnnotation(member0.owner.typeSignature, Seq(name0, name1, name2), typeOf[transient]).nonEmpty
          val hasJson = getAnnotationValueOrElse("json", false)
          val hasSerialize = getAnnotationValueOrElse("serialize", false)
          val hasIndexed = getAnnotationValueOrElse("indexed", false)
          val hasVersion = getAnnotationValueOrElse("version", false)
          val hasCreationDate = getAnnotationValueOrElse("creationDate", false)
          val hasModificationDate = getAnnotationValueOrElse("modificationDate", false)
          val order = getAnnotationValueOrElse("order", (entityType.baseClasses.indexOf(member0.owner) + 1) * 1000)

          val tpe = member0.asMethod.returnType
          val p = PropertyInfo(
            name0,
            tpe,
            order,
            tpe.baseClasses.map(_.asType.fullName).toSeq,
            serializable = hasSerialize && tpe <:< typeOf[Serializable],
            json = hasJson,
            transient = hasTransient,
            indexed = hasIndexed,
            version = hasVersion,
            creationDate = hasCreationDate,
            modificationDate = hasModificationDate,
            readonly = !member1.isMethod,
            listener = Nil
          )
          //      println("PropertyInfo: " + p)
          Some(p)
      }
    }

    def isMemberOfEntity(members: Seq[c.Symbol]): Boolean = members.filterNot(_ == NoSymbol).exists(_isMemberOfEntity)

    def _isMemberOfEntity(member: c.Symbol): Boolean = {
      member.owner.annotations.exists(_.tree.tpe == typeOf[entity]) || member.owner.asType.typeSignature.baseClasses.tail.exists(_.fullName == "com.xhachi.gae4s.datastore.Entity")
    }

    // TODO: Listenerの仕組みはtraitのフィールドのアノテーションを取得できるようになってから
    //  def getListener(member: c.Symbol): Option[] = {
    //    import c.universe._
    //    val tail = member.annotations.find(_.tree.tpe == typeOf[listener]).map(_.tree.children.tail)
    //    null
    //  }

    case class TypeDesc(tpe: Symbol, ta: Seq[Type] = Nil)

    def toTypeDesc(typeName: String, typeArg: Seq[Type] = Nil) = TypeDesc(c.mirror.staticClass(typeName), typeArg)

    case class PropertyDesc(base: TypeDesc, args: Seq[Tree], withTrait: Seq[TypeDesc] = Nil, body: Seq[Tree] = Nil) {

      def treeAsInstance = (base.ta, withTrait) match {

        //withなし
        case (Nil, Nil) =>
          q"""new ${base.tpe}(..$args) {..$body}"""
        case (_, Nil) =>
          q"""new ${base.tpe}[..${base.ta}](..$args) {..$body}"""

        //withが1つ
        case (Nil, Seq(t1@TypeDesc(_, Nil))) =>
          q"""new ${base.tpe}(..$args) with ${t1.tpe} {..$body}"""
        case (Nil, Seq(t1@TypeDesc(_, _))) =>
          q"""new ${base.tpe}(..$args) with ${t1.tpe}[..${t1.ta}] {..$body}"""
        case (_, Seq(t1@TypeDesc(_, Nil))) =>
          q"""new ${base.tpe}[..${base.ta}](..$args) with ${t1.tpe} {..$body}"""
        case (_, Seq(t1@TypeDesc(_, _))) =>
          q"""new ${base.tpe}[..${base.ta}](..$args) with ${t1.tpe}[..${t1.ta}] {..$body}"""

        //withが2つ
        case (Nil, Seq(t1@TypeDesc(_, Nil), t2@TypeDesc(_, Nil))) =>
          q"""new ${base.tpe}(..$args) with ${t1.tpe} with ${t2.tpe} {..$body}"""
        case (Nil, Seq(t1@TypeDesc(_, Nil), t2@TypeDesc(_, _))) =>
          q"""new ${base.tpe}(..$args) with ${t1.tpe} with ${t2.tpe}[..${t2.ta}] {..$body}"""
        case (Nil, Seq(t1@TypeDesc(_, _), t2@TypeDesc(_, Nil))) =>
          q"""new ${base.tpe}(..$args) with ${t1.tpe}[..${t1.ta}] with ${t2.tpe}{..$body}"""
        case (Nil, Seq(t1@TypeDesc(_, _), t2@TypeDesc(_, _))) =>
          q"""new ${base.tpe}(..$args) with ${t1.tpe}[..${t1.ta}] with ${t2.tpe}[..${t2.ta}] {..$body}"""
        case (_, Seq(t1@TypeDesc(_, Nil), t2@TypeDesc(_, Nil))) =>
          q"""new ${base.tpe}[..${base.ta}](..$args) with ${t1.tpe} with ${t2.tpe} {..$body}"""
        case (_, Seq(t1@TypeDesc(_, Nil), t2@TypeDesc(_, _))) =>
          q"""new ${base.tpe}[..${base.ta}](..$args) with ${t1.tpe} with ${t2.tpe}[..${t2.ta}] {..$body}"""
        case (_, Seq(t1@TypeDesc(_, _), t2@TypeDesc(_, Nil))) =>
          q"""new ${base.tpe}[..${base.ta}](..$args) with ${t1.tpe}[..${t1.ta}] with ${t2.tpe} {..$body}"""
        case (_, Seq(t1@TypeDesc(_, _), t2@TypeDesc(_, _))) =>
          q"""new ${base.tpe}[..${base.ta}](..$args) with ${t1.tpe}[..${t1.ta}] with ${t2.tpe}[..${t2.ta}] {..$body}"""

        //withが3つ
        case (Nil, Seq(t1@TypeDesc(_, Nil), t2@TypeDesc(_, Nil), t3@TypeDesc(_, Nil))) =>
          q"""new ${base.tpe}(..$args) with ${t1.tpe} with ${t2.tpe} with ${t3.tpe} {..$body}"""
        case (Nil, Seq(t1@TypeDesc(_, Nil), t2@TypeDesc(_, Nil), t3@TypeDesc(_, _))) =>
          q"""new ${base.tpe}(..$args) with ${t1.tpe} with ${t2.tpe} with ${t3.tpe}[..${t3.ta}] {..$body}"""
        case (Nil, Seq(t1@TypeDesc(_, Nil), t2@TypeDesc(_, _), t3@TypeDesc(_, Nil))) =>
          q"""new ${base.tpe}(..$args) with ${t1.tpe} with ${t2.tpe}[..${t2.ta}] with ${t3.tpe} {..$body}"""
        case (Nil, Seq(t1@TypeDesc(_, Nil), t2@TypeDesc(_, _), t3@TypeDesc(_, _))) =>
          q"""new ${base.tpe}(..$args) with ${t1.tpe} with ${t2.tpe}[..${t2.ta}] with ${t3.tpe}[..${t3.ta}] {..$body}"""
        case (Nil, Seq(t1@TypeDesc(_, _), t2@TypeDesc(_, Nil), t3@TypeDesc(_, Nil))) =>
          q"""new ${base.tpe}(..$args) with ${t1.tpe}[..${t1.ta}] with ${t2.tpe} with ${t3.tpe} {..$body}"""
        case (Nil, Seq(t1@TypeDesc(_, _), t2@TypeDesc(_, Nil), t3@TypeDesc(_, _))) =>
          q"""new ${base.tpe}(..$args) with ${t1.tpe}[..${t1.ta}] with ${t2.tpe} with ${t3.tpe}[..${t3.ta}] {..$body}"""
        case (Nil, Seq(t1@TypeDesc(_, _), t2@TypeDesc(_, _), t3@TypeDesc(_, Nil))) =>
          q"""new ${base.tpe}(..$args) with ${t1.tpe}[..${t1.ta}] with ${t2.tpe}[..${t2.ta}] with ${t3.tpe} {..$body}"""
        case (Nil, Seq(t1@TypeDesc(_, _), t2@TypeDesc(_, _), t3@TypeDesc(_, _))) =>
          q"""new ${base.tpe}(..$args) with ${t1.tpe}[..${t1.ta}] with ${t2.tpe}[..${t2.ta}] with ${t3.tpe}[..${t3.ta}] {..$body}"""
        case (_, Seq(t1@TypeDesc(_, Nil), t2@TypeDesc(_, Nil), t3@TypeDesc(_, Nil))) =>
          q"""new ${base.tpe}[..${base.ta}](..$args) with ${t1.tpe} with ${t2.tpe} with ${t3.tpe} {..$body}"""
        case (_, Seq(t1@TypeDesc(_, Nil), t2@TypeDesc(_, Nil), t3@TypeDesc(_, _))) =>
          q"""new ${base.tpe}[..${base.ta}](..$args) with ${t1.tpe} with ${t2.tpe} with ${t3.tpe}[..${t3.ta}] {..$body}"""
        case (_, Seq(t1@TypeDesc(_, Nil), t2@TypeDesc(_, _), t3@TypeDesc(_, Nil))) =>
          q"""new ${base.tpe}[..${base.ta}](..$args) with ${t1.tpe} with ${t2.tpe}[..${t2.ta}] with ${t3.tpe} {..$body}"""
        case (_, Seq(t1@TypeDesc(_, Nil), t2@TypeDesc(_, _), t3@TypeDesc(_, _))) =>
          q"""new ${base.tpe}[..${base.ta}](..$args) with ${t1.tpe} with ${t2.tpe}[..${t2.ta}] with ${t3.tpe}[..${t3.ta}] {..$body}"""
        case (_, Seq(t1@TypeDesc(_, _), t2@TypeDesc(_, Nil), t3@TypeDesc(_, Nil))) =>
          q"""new ${base.tpe}[..${base.ta}](..$args) with ${t1.tpe}[..${t1.ta}] with ${t2.tpe} with ${t3.tpe} {..$body}"""
        case (_, Seq(t1@TypeDesc(_, _), t2@TypeDesc(_, Nil), t3@TypeDesc(_, _))) =>
          q"""new ${base.tpe}[..${base.ta}](..$args) with ${t1.tpe}[..${t1.ta}] with ${t2.tpe} with ${t3.tpe}[..${t3.ta}] {..$body}"""
        case (_, Seq(t1@TypeDesc(_, _), t2@TypeDesc(_, _), t3@TypeDesc(_, Nil))) =>
          q"""new ${base.tpe}[..${base.ta}](..$args) with ${t1.tpe}[..${t1.ta}] with ${t2.tpe}[..${t2.ta}] with ${t3.tpe} {..$body}"""
        case (_, Seq(t1@TypeDesc(_, _), t2@TypeDesc(_, _), t3@TypeDesc(_, _))) =>
          q"""new ${base.tpe}[..${base.ta}](..$args) with ${t1.tpe}[..${t1.ta}] with ${t2.tpe}[..${t2.ta}] with ${t3.tpe}[..${t3.ta}] {..$body}"""
        case _ =>
          c.abort(c.enclosingPosition, s"Invalid TypeDesc. " + this)
      }
    }



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

        val p = if (info.version) {
          PropertyDesc(toTypeDesc("com.xhachi.gae4s.datastore.VersionProperty"), Seq(q"""${info.stringName}"""))
        } else if (info.creationDate) {
          PropertyDesc(toTypeDesc("com.xhachi.gae4s.datastore.CreationDateProperty"), Seq(q"""${info.stringName}"""))
        } else if (info.modificationDate) {
          PropertyDesc(toTypeDesc("com.xhachi.gae4s.datastore.ModificationDateProperty"), Seq(q"""${info.stringName}"""))
        } else {
          def getPropertyDesc(t: Type): PropertyDesc = if (info.json) {
            PropertyDesc(toTypeDesc("com.xhachi.gae4s.datastore.JsonProperty", Seq(info.storeType)), Seq(q"""${info.stringName}"""))
          } else if (info.serializable) {
            PropertyDesc(toTypeDesc("com.xhachi.gae4s.datastore.SerializableProperty", Seq(info.storeType)), Seq(q"""${info.stringName}"""))
          } else if (isValueType(t)) {
            val propertyTypeName = TypeName(t.typeSymbol.asType.name.toTypeName + "Property")
            PropertyDesc(toTypeDesc(s"com.xhachi.gae4s.datastore.$propertyTypeName"), Seq(q"""${info.stringName}"""))
          } else if (t =:= typeOf[String]) {
            PropertyDesc(toTypeDesc("com.xhachi.gae4s.datastore.StringProperty"), Seq(q"""${info.stringName}"""))
          } else if (info.storeType =:= typeOf[Array[Byte]]) {
            PropertyDesc(toTypeDesc("com.xhachi.gae4s.datastore.ByteArrayProperty"), Seq(q"""${info.stringName}"""))
          } else if (t =:= typeOf[Double]) {
            PropertyDesc(toTypeDesc("com.xhachi.gae4s.datastore.DoubleProperty"), Seq(q"""${info.stringName}"""))
          } else if (t =:= typeOf[Int]) {
            PropertyDesc(toTypeDesc("com.xhachi.gae4s.datastore.IntProperty"), Seq(q"""${info.stringName}"""))
          } else if (t =:= typeOf[Long]) {
            PropertyDesc(toTypeDesc("com.xhachi.gae4s.datastore.LongProperty"), Seq(q"""${info.stringName}"""))
          } else if (info.isKey) {
            PropertyDesc(toTypeDesc("com.xhachi.gae4s.datastore.KeyProperty", Seq(info.keyType)), Seq(q"""${info.stringName}"""))
          } else if (info.isJavaEnum) {
            PropertyDesc(toTypeDesc("com.xhachi.gae4s.datastore.EnumProperty", Seq(info.storeType)), Seq(q"""${info.stringName}"""))
          } else if (info.isScalaEnum) {
            val enum = c.mirror.staticModule(info.scalaEnumName)
            PropertyDesc(toTypeDesc("com.xhachi.gae4s.datastore.EnumerationProperty", Seq(info.storeType)),
              Seq(q"""${info.stringName}"""),
              body = Seq(
                q"def withName(name: String): $enum.Value = $enum.withName(name)",
                q"def values: Seq[$enum.Value] = $enum.values.toSeq",
                q"""
def fromString(value: String): $enum.Value = value match {
  case v: String => $enum.withName(value)
  case _ => null.asInstanceOf[$enum.Value]
}
                   """,
                q"""
def toString(value: $enum.Value): String = value match {
  case v: $enum.Value => value.toString
  case _ => null.asInstanceOf[String]
}
"""
              ))
          } else {
            c.abort(c.enclosingPosition, s"${info.name} as ${info.storeType} cannot be property\n\n" + info)
          }

          val p0 = getPropertyDesc(info.storeType)
          val p1 = if (info.isOption) {
            PropertyDesc(toTypeDesc("com.xhachi.gae4s.datastore.OptionProperty", Seq(info.storeType)), Seq(p0.treeAsInstance))
          } else if (info.isSeq && !info.json) {
            PropertyDesc(toTypeDesc("com.xhachi.gae4s.datastore.SeqProperty", Seq(info.storeType)), Seq(p0.treeAsInstance))
          } else {
            p0
          }

          val p2 = if (info.indexed) {
            p1.copy(withTrait = Seq(toTypeDesc("com.xhachi.gae4s.datastore.IndexedProperty", Seq(info.tpe))))
          } else {
            p1
          }
          p2
        }

        val p1 = p.copy(
          withTrait = p.withTrait :+ toTypeDesc("com.xhachi.gae4s.datastore.Getter", Seq(entityType, info.tpe)),
          body = p.body :+ q"def getValueFromEntity(e: $entityType) = e.${info.name}"
        )

        val p2 = if (!info.readonly) {
          p1.copy(
            withTrait = p1.withTrait :+ toTypeDesc("com.xhachi.gae4s.datastore.Setter", Seq(entityType, info.tpe)),
            body = p1.body :+ q"def setValueToEntity(e: $entityType, v: ${info.tpe}) = e.${info.name} = v"
          )
        } else {
          p1
        }

        Some(info.name -> p2.treeAsInstance)
      } else {
        None
      }
    }

    val propertyInfos = entityType.baseClasses.map(entityType.baseType(_).decls).flatten
      .filter(m => m.isMethod && m.asMethod.paramLists.isEmpty)
      .filter(_.name.encodedName.toString != "key")
      .map(_.name.toTermName)
      .distinct
      .flatMap(toPropertyInfo)
      .sortWith((a, b) => a.order < b.order)

    val normalProperties = propertyInfos.filterNot(_.version)
      .map(i => toProperty(i))
      .toSeq
      .filter(_.isDefined).map(_.get)

    val versionProperty = propertyInfos.filter(_.version)
      .map(i => toProperty(i))
      .toSeq
      .filter(_.isDefined).map(_.get)
      .headOption

    val properties = normalProperties ++ versionProperty.toSeq

    val fields = properties.map {
      case (n, v) => q"""val $n = $v"""
    }

    val names = properties.map(_._1)
    val versionName = versionProperty.map(_._1)

    val ancestorTypeTree = ancestorType match {
      case Some(a) => q"val ancestorType = Some(classOf[$a])"
      case None => q"val ancestorType = None"
    }

    val tree = q"""
class $metaName extends com.xhachi.gae4s.datastore.EntityMeta[$entityType] {

  def entityType = classOf[$entityType]

  $ancestorTypeTree

  ..$fields

  override val properties = Seq(
  ..$names
  )

  override val versionProperty: Option[com.xhachi.gae4s.datastore.VersionProperty] = $versionName

  def createEntity(key: com.xhachi.gae4s.datastore.Key[$entityType]) = new $entityType(key)

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
  def printDebug(tree: c.Tree): Unit = {
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


  def getAnnotation(target: c.Type, names: Seq[c.TermName], tpe: c.Type): Seq[c.universe.Annotation] = {
    target
      .baseClasses
      .flatMap(b => getAnnotationInConstructorParam(b, names, tpe) ++ getAnnotationInMember(b, names, tpe))
  }

  private def getAnnotationInMember(target: c.Symbol, names: Seq[c.TermName], tpe: c.Type): Seq[c.universe.Annotation] = {
    names
      .map(target.typeSignature.decl)
      .flatMap(m => m.annotations.filter(_.tree.tpe =:= tpe))
  }

  private def getAnnotationInConstructorParam(target: c.Symbol, names: Seq[c.TermName], tpe: c.Type): Seq[c.universe.Annotation] = {
    import c.universe._

    target match {
      case o: TypeSymbol if o.typeSignature.member(termNames.CONSTRUCTOR).isMethod =>
        o.typeSignature.member(termNames.CONSTRUCTOR).asMethod
          .paramLists
          .flatMap(p => p.filter(s => names.contains(s.name)))
          .flatMap(_.annotations.filter(_.tree.tpe =:= tpe))
      case _ => Nil
    }
  }


}
package com.xhachi.gae4s.datastore

import java.util.Date

import scala.reflect.ClassTag
import scala.Some

import com.google.appengine.api.datastore.{Key => LLKey}
import com.google.appengine.api.datastore.{Entity => LLEntity}
import com.google.appengine.api.datastore.Query.FilterOperator._
import com.google.appengine.api.datastore.Query.SortDirection._
import com.google.appengine.api.datastore._
import com.google.appengine.api.users._
import com.google.appengine.api.blobstore._

object Property {
  val ShortLimit = 500
  val LongLimit = 1000000
}

trait Property[T] {

  protected[datastore] def name: String

  def getValueFromLLEntity(entity: LLEntity): T = fromStoreProperty(entity.getProperty(name))

  def setValueToLLEntity(entity: LLEntity)(value: T) = this match {
    case p: IndexedProperty[_] => entity.setProperty(name, toStoreProperty(value))
    case _ => entity.setUnindexedProperty(name, toStoreProperty(value))
  }

  protected[datastore] def toStoreProperty(value: T): Any

  protected[datastore] def fromStoreProperty(value: Any): T

  protected[datastore] def createFromConversionException(v: Any) = new PropertyConversionException(name + " \"" + v + "\"(" + v.getClass.getName + ") can not convert from stored property")

  protected[datastore] def createToConversionException(v: Any) = new PropertyConversionException(name + " \"\"+v+\"\"(\" + v.getClass.getName + \") can not convert to store property")
}

trait IndexedProperty[Q] {

  protected[datastore] def name: String

  def #==(value: Q): Filter = FilterPredicate(name, EQUAL, value)

  def #!=(value: Q): Filter = FilterPredicate(name, NOT_EQUAL, value)

  def in(value: Q, values: Q*): Filter = FilterPredicate(name, IN, value :: values ++: Nil)

  def #>(value: Q): Filter = FilterPredicate(name, GREATER_THAN, value)

  def #>=(value: Q): Filter = FilterPredicate(name, GREATER_THAN_OR_EQUAL, value)

  def #<(value: Q): Filter = FilterPredicate(name, LESS_THAN, value)

  def #<=(value: Q): Filter = FilterPredicate(name, LESS_THAN_OR_EQUAL, value)

  def asc = SortPredicate(name, ASCENDING)

  def desc = SortPredicate(name, DESCENDING)
}

class PropertyConversionException(message: String) extends Exception(message)


trait SimpleProperty[T] extends Property[T] {

  override def toStoreProperty(value: T): Any = value

  override def fromStoreProperty(value: Any): T = value.asInstanceOf[T]
}

class OptionProperty[T](property: Property[T]) extends Property[Option[T]] {

  protected[datastore] def name = property.name

  override protected[datastore] def fromStoreProperty(value: Any): Option[T] = property.fromStoreProperty(value) match {
    case null => None
    case v => Some(v)
  }

  override protected[datastore] def toStoreProperty(value: Option[T]): Any = value match {
    case Some(v) => property.toStoreProperty(v)
    case _ => null
  }
}

class KeyProperty[E <: Entity[E]](protected[datastore] val name: String) extends Property[Key[E]] with IndexedProperty[Key[E]] {

  override protected[datastore] def fromStoreProperty(value: Any): Key[E] = value match {
    //TODO: あとで
    case k: LLKey => new Key[E](k)
    case _ => null
  }

  override protected[datastore] def toStoreProperty(value: Key[E]): Any = value.key
}

class LongProperty(protected[datastore] val name: String) extends SimpleProperty[Long] {
  override def toStoreProperty(value: Long): Any = value

  override def fromStoreProperty(value: Any): Long = value match {
    case v: Long => v
    case v: Double => v.toLong
    case v => throw createFromConversionException(v)
  }
}

class IntProperty(protected[datastore] val name: String) extends SimpleProperty[Int] {
  override def toStoreProperty(value: Int): Any = value

  override def fromStoreProperty(value: Any): Int = value match {
    case v: Long => v.toInt
    case v: Double => v.toInt
    case v => throw createFromConversionException(v)
  }
}

class DoubleProperty(protected[datastore] val name: String) extends SimpleProperty[Double]

class BooleanProperty(protected[datastore] val name: String) extends SimpleProperty[Boolean]

class DateProperty(protected[datastore] val name: String) extends SimpleProperty[Date]

class GeoPtProperty(protected[datastore] val name: String) extends SimpleProperty[GeoPt]

class ShortBlobProperty(protected[datastore] val name: String) extends SimpleProperty[ShortBlob]

class BlobProperty(protected[datastore] val name: String) extends SimpleProperty[Blob]

class PostalAddressProperty(protected[datastore] val name: String) extends SimpleProperty[PostalAddress]

class PhoneNumberProperty(protected[datastore] val name: String) extends SimpleProperty[PhoneNumber]

class EmailProperty(protected[datastore] val name: String) extends SimpleProperty[Email]

class UserProperty(protected[datastore] val name: String) extends SimpleProperty[User]

class IMHandleProperty(protected[datastore] val name: String) extends SimpleProperty[IMHandle]

class LinkProperty(protected[datastore] val name: String) extends SimpleProperty[Link]

class CategoryProperty(protected[datastore] val name: String) extends SimpleProperty[Category]

class RatingProperty(protected[datastore] val name: String) extends SimpleProperty[Rating]

class BlobKeyProperty(protected[datastore] val name: String) extends SimpleProperty[BlobKey]


trait StringStoreProperty[P] extends SimpleProperty[P] {

  def toString(value: P): String

  def fromString(value: String): P

  final override def toStoreProperty(p: P): Any = {
    toString(p) match {
      case value: String if value.length < Property.ShortLimit => value
      case value: String if value.length < Property.LongLimit => new Text(value)
      case value: String => throw new PropertyConversionException(name + " property is too long")
      case _ => null
    }
  }

  final override def fromStoreProperty(value: Any): P = {
    val p = value match {
      case value: Text => value.getValue
      case value: String => value
      case value: Any => value.toString
      case _ => null
    }
    fromString(p)
  }
}

class StringProperty(protected[datastore] val name: String) extends StringStoreProperty[String] {

  override def fromString(value: String): String = value

  override def toString(value: String): String = value
}

class BigIntProperty(protected[datastore] val name: String) extends StringStoreProperty[BigInt] {

  override def fromString(value: String): BigInt = BigInt(value)

  override def toString(value: BigInt): String = value.toString()
}

class BigDecimalProperty(protected[datastore] val name: String) extends StringStoreProperty[BigDecimal] {

  override def fromString(value: String): BigDecimal = BigDecimal(value)

  override def toString(value: BigDecimal): String = value.toString()
}


class EnumProperty[E <: Enum[E] : ClassTag](val name: String) extends StringStoreProperty[E] {
  private def enumValueOf[T <: Enum[T]](cls: Class[_], stringValue: String): Enum[_] =
    Enum.valueOf(cls.asInstanceOf[Class[T]], stringValue).asInstanceOf[Enum[_]]

  override def fromString(value: String): E = {

    implicitly[ClassTag[E]].runtimeClass match {
      case clz if classOf[Enum[E]].isAssignableFrom(clz) => enumValueOf(clz, value).asInstanceOf[E]
      case _ => throw new RuntimeException("Enum type error.")
    }
  }

  override def toString(value: E): String = value.name()
}


trait ByteProperty[B] extends SimpleProperty[B] {

  protected def toByte(value: B): Array[Byte]

  protected def fromByte(value: Array[Byte]): B

  override def toStoreProperty(b: B): Any = {
    val value = toByte(b)
    if (value.length < Property.ShortLimit) new ShortBlob(value)
    else if (value.length < Property.LongLimit) new ShortBlob(value)
    else throw createToConversionException(value)
  }

  override def fromStoreProperty(value: Any): B = {
    val b: Array[Byte] = value match {
      case value: ShortBlob => value.getBytes
      case value: Blob => value.getBytes
      case value: Any => throw createFromConversionException(value)
      case _ => null
    }
    fromByte(b)
  }
}


class ByteArrayProperty(protected[datastore] val name: String) extends ByteProperty[Array[Byte]] {

  override def fromByte(value: Array[Byte]): Array[Byte] = value

  override def toByte(value: Array[Byte]): Array[Byte] = value
}

class SerializableProperty[E <: Serializable](protected[datastore] val name: String) extends ByteProperty[E] {

  import java.io._

  override def fromByte(value: Array[Byte]): E = {
    val bais = new ByteArrayInputStream(value)
    val ois = new ObjectInputStream(bais)
    val instance = ois.readObject().asInstanceOf[E]
    ois.close()
    bais.close()
    instance
  }

  override def toByte(value: E): Array[Byte] = {
    val baos = new ByteArrayOutputStream
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(value)
    val bytes = baos.toByteArray
    oos.close()
    baos.close()
    bytes
  }
}

class JsonProperty[E <: AnyRef : Manifest](protected[datastore] val name: String) extends StringStoreProperty[E] {

  import org.json4s._
  import org.json4s.native.Serialization.write
  import org.json4s.native.JsonMethods._

  implicit var formats = JsonProperty.formats

  override def fromString(value: String): E = parse(value).extract[E]

  override def toString(value: E): String = write[E](value)
}

object JsonProperty {

  import org.json4s._

  private var formats: Formats = DefaultFormats

  def :=(serializer: Serializer[_]): Unit = {
    formats = formats + serializer
  }

}

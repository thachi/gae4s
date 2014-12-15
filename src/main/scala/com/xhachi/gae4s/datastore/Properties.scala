package com.xhachi.gae4s.datastore

import java.util.Date

import com.google.appengine.api.blobstore._
import com.google.appengine.api.datastore.Query.FilterOperator._
import com.google.appengine.api.datastore.Query.SortDirection._
import com.google.appengine.api.datastore.{Entity => LLEntity, Key => LLKey, _}
import com.google.appengine.api.users._
import com.xhachi.gae4s.json.Json

import scala.reflect.ClassTag

object Property {
  val ShortLimit = 500
  val LongLimit = 1000000
}

abstract class Property[T: ClassTag] {

  def propertyType = implicitly[ClassTag[T]].runtimeClass

  def name: String

  def getValueFromLLEntity(entity: LLEntity): T = fromStoreProperty(entity.getProperty(name))

  def setValueToLLEntity(entity: LLEntity)(value: T) = this match {
    case p: IndexedProperty[_] => entity.setProperty(name, toStoreProperty(value))
    case _ => entity.setUnindexedProperty(name, toStoreProperty(value))
  }

  protected[datastore] def toStoreProperty(value: T): Any

  protected[datastore] def fromStoreProperty(value: Any): T

}

trait IndexedProperty[T] extends Property[T] {

  def name: String

  val compare: (T, T) => Int = (v1, v2) => v1.asInstanceOf[Comparable[T]].compareTo(v2)

  def #==(value: T): Filter = FilterPredicate(name, EQUAL, this, value)

  def #!=(value: T): Filter = FilterPredicate(name, NOT_EQUAL, this, value)

  def in(value: T, values: T*): Filter = FilterPredicate(name, IN, this, value, values)

  def #>(value: T): Filter = FilterPredicate(name, GREATER_THAN, this, value)

  def #>=(value: T): Filter = FilterPredicate(name, GREATER_THAN_OR_EQUAL, this, value)

  def #<(value: T): Filter = FilterPredicate(name, LESS_THAN, this, value)

  def #<=(value: T): Filter = FilterPredicate(name, LESS_THAN_OR_EQUAL, this, value)

  def asc = SortPredicate(name, ASCENDING, this)

  def desc = SortPredicate(name, DESCENDING, this)
}

class PropertyConversionException(message: String) extends Exception(message)

class PropertyConvertFromLLPropertyException(name: String, value: Any)
  extends PropertyConversionException( s"""$name "$value" can not converte from stored property""")

class PropertyConvertToLLPropertyException(name: String, value: Any)
  extends PropertyConversionException( s"""$name "$value" can not converte to store property""")


trait SimpleProperty[T] extends Property[T] {

  override def toStoreProperty(value: T): Any = value

  override def fromStoreProperty(value: Any): T = value.asInstanceOf[T]
}

class OptionProperty[T](property: Property[T]) extends Property[Option[T]] {

  def name = property.name

  override protected[datastore] def fromStoreProperty(value: Any): Option[T] = property.fromStoreProperty(value) match {
    case null => None
    case v => Some(v)
  }

  override protected[datastore] def toStoreProperty(value: Option[T]): Any = value match {
    case Some(v) => property.toStoreProperty(v)
    case _ => null
  }
}

class KeyProperty[E <: Entity[E]](val name: String) extends Property[Key[E]] with IndexedProperty[Key[E]] {

  override protected[datastore] def fromStoreProperty(value: Any): Key[E] = value match {
    //TODO: あとで
    case k: LLKey => new Key[E](k)
    case _ => null
  }

  override protected[datastore] def toStoreProperty(value: Key[E]): Any = value match {
    case v: Key[E] => value.key
    case _ => null
  }

  override val compare: (Key[E], Key[E]) => Int = (v1, v2) => v1.key.compareTo(v2.key)
}

class LongProperty(val name: String) extends SimpleProperty[Long] {
  override def toStoreProperty(value: Long): Any = value

  override def fromStoreProperty(value: Any): Long = value match {
    case v: Int => v.toLong
    case v: Long => v
    case v: Double => v.toLong
    case null => 0
    case v => throw new PropertyConvertFromLLPropertyException(name, v)
  }
}

class IntProperty(val name: String) extends SimpleProperty[Int] {
  override def toStoreProperty(value: Int): Any = value

  override def fromStoreProperty(value: Any): Int = value match {
    case v: Int => v
    case v: Long => v.toInt
    case v: Double => v.toInt
    case null => 0
    case v => throw new PropertyConvertFromLLPropertyException(name, v)
  }
}

class DoubleProperty(val name: String) extends SimpleProperty[Double]

class BooleanProperty(val name: String) extends SimpleProperty[Boolean]

class DateProperty(val name: String) extends SimpleProperty[Date]

class GeoPtProperty(val name: String) extends SimpleProperty[GeoPt]

class ShortBlobProperty(val name: String) extends SimpleProperty[ShortBlob]

class BlobProperty(val name: String) extends SimpleProperty[Blob]

class PostalAddressProperty(val name: String) extends SimpleProperty[PostalAddress]

class PhoneNumberProperty(val name: String) extends SimpleProperty[PhoneNumber]

class EmailProperty(val name: String) extends SimpleProperty[Email]

class UserProperty(val name: String) extends SimpleProperty[User]

class IMHandleProperty(val name: String) extends SimpleProperty[IMHandle]

class LinkProperty(val name: String) extends SimpleProperty[Link]

class CategoryProperty(val name: String) extends SimpleProperty[Category]

class RatingProperty(val name: String) extends SimpleProperty[Rating]

class BlobKeyProperty(val name: String) extends SimpleProperty[BlobKey]


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

class StringProperty(val name: String) extends StringStoreProperty[String] {

  override def fromString(value: String): String = value

  override def toString(value: String): String = value
}

class BigIntProperty(val name: String) extends StringStoreProperty[BigInt] {

  override def fromString(value: String): BigInt = BigInt(value)

  override def toString(value: BigInt): String = value.toString()
}

class BigDecimalProperty(val name: String) extends StringStoreProperty[BigDecimal] {

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
    else if (value.length < Property.LongLimit) new Blob(value)
    else throw new PropertyConvertToLLPropertyException(name, value)
  }

  override def fromStoreProperty(value: Any): B = {
    val b: Array[Byte] = value match {
      case value: ShortBlob => value.getBytes
      case value: Blob => value.getBytes
      case value: Any => throw new PropertyConvertToLLPropertyException(name, value)
      case _ => null
    }
    fromByte(b)
  }
}


class ByteArrayProperty(val name: String) extends ByteProperty[Array[Byte]] {

  override def fromByte(value: Array[Byte]): Array[Byte] = value

  override def toByte(value: Array[Byte]): Array[Byte] = value
}

class SerializableProperty[E <: Serializable: ClassTag](val name: String) extends ByteProperty[E] {

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

class JsonProperty[E <: AnyRef : Manifest](val name: String) extends StringStoreProperty[E] {

  override def fromString(value: String): E = Json.parseAs[E](value)

  override def toString(value: E): String = Json.stringify(value)
}

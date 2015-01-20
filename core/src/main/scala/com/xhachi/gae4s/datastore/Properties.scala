package com.xhachi.gae4s.datastore

import java.util.Date

import com.google.appengine.api.blobstore.BlobKey
import com.google.appengine.api.datastore.Query.FilterOperator._
import com.google.appengine.api.datastore.Query.SortDirection._
import com.google.appengine.api.datastore.{Entity => LLEntity, Key => LLKey, _}
import com.google.appengine.api.users.User
import com.xhachi.gae4s.json.Json

import scala.reflect.ClassTag

object Property {
  val ShortLimit = 500
  val LongLimit = 1000000
}

abstract class Property[T: ClassTag] extends Serializable {

  type PropertyType = T

  def propertyType: Class[_] = implicitly[ClassTag[T]].runtimeClass

  def name: String

  def getValueFromLLEntity(entity: LLEntity): T = fromStoreProperty(entity.getProperty(name))

  def setValueToLLEntity(entity: LLEntity)(value: T) = this match {
    case p: IndexedProperty[_] => entity.setProperty(name, toStoreProperty(value))
    case _ => entity.setUnindexedProperty(name, toStoreProperty(value))
  }

  protected[datastore] def toStoreProperty(value: T): Any

  protected[datastore] def fromStoreProperty(value: Any): T

  override def toString = s"(${getClass.getName}($name)})"

}

trait IndexedProperty[T] extends Property[T] {

  def name: String

  val compare: (T, T) => Int = (v1, v2) => v1.asInstanceOf[Comparable[T]].compareTo(v2)


  def isEqual(value: T): Filter = FilterPredicate(name, EQUAL, this, value)

  def isNotEqual(value: T): Filter = FilterPredicate(name, NOT_EQUAL, this, value)

  def isGreaterThan(value: T): Filter = FilterPredicate(name, GREATER_THAN, this, value)

  def isGreaterThanOrEqual(value: T): Filter = FilterPredicate(name, GREATER_THAN_OR_EQUAL, this, value)

  def isLessThan(value: T): Filter = FilterPredicate(name, LESS_THAN, this, value)

  def isLessThanOrEqual(value: T): Filter = FilterPredicate(name, LESS_THAN_OR_EQUAL, this, value)

  def #==(value: T): Filter = isEqual(value)

  def #!=(value: T): Filter = isNotEqual(value)

  def #>(value: T): Filter = isGreaterThan(value)

  def #>=(value: T): Filter = isGreaterThanOrEqual(value)

  def #<(value: T): Filter = isLessThan(value)

  def #<=(value: T): Filter = isLessThanOrEqual(value)

  def in(value: T, values: T*): Filter = FilterPredicate(name, IN, this, value, values)

  def asc = SortPredicate(name, ASCENDING, this)

  def desc = SortPredicate(name, DESCENDING, this)
}

class PropertyConversionException(message: String) extends Exception(message)

class PropertyConvertFromLLPropertyException(name: String, value: Any)
  extends PropertyConversionException( s"""$name "$value" can not convert from stored property""")

class PropertyConvertToLLPropertyException(name: String, value: Any)
  extends PropertyConversionException( s"""$name "$value" can not convert to store property""")


class ValueProperty[T: ClassTag](val name: String) extends Property[T] {

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

class SeqProperty[T](property: Property[T]) extends Property[Seq[T]] {

  def name = property.name

  override protected[datastore] def fromStoreProperty(value: Any): Seq[T] = property.fromStoreProperty(value) match {
    case null => Nil
    case v: Array[T] => v.toSeq
  }

  override protected[datastore] def toStoreProperty(value: Seq[T]): Any = value match {
    case Nil => null
    case v: Seq[T] => v.map(property.toStoreProperty).toArray
  }
}

class KeyProperty[E <: Entity[E]](val name: String) extends Property[Key[E]] with IndexedProperty[Key[E]] {

  override protected[datastore] def fromStoreProperty(value: Any): Key[E] = value match {
    case k: LLKey => Key[E](k)
    case _ => null
  }

  override protected[datastore] def toStoreProperty(value: Key[E]): Any = value match {
    case v: Key[E] => value.key
    case _ => null
  }

  override val compare: (Key[E], Key[E]) => Int = (v1, v2) => v1.key.compareTo(v2.key)
}

class LongProperty(name: String) extends ValueProperty[Long](name) {
  override def toStoreProperty(value: Long): Any = value

  override def fromStoreProperty(value: Any): Long = value match {
    case v: Int => v.toLong
    case v: Long => v
    case v: Double => v.toLong
    case null => 0
    case v => throw new PropertyConvertFromLLPropertyException(name, v)
  }
}

class IntProperty(name: String) extends ValueProperty[Int](name) {
  override def toStoreProperty(value: Int): Any = value

  override def fromStoreProperty(value: Any): Int = value match {
    case v: Int => v
    case v: Long => v.toInt
    case null => 0
    case v => throw new PropertyConvertFromLLPropertyException(name, v)
  }
}


class DoubleProperty(name: String) extends ValueProperty[Double](name)

class BooleanProperty(name: String) extends ValueProperty[Boolean](name)

class DateProperty(name: String) extends ValueProperty[Date](name)

class GeoPtProperty(name: String) extends ValueProperty[GeoPt](name)

class ShortBlobProperty(name: String) extends ValueProperty[ShortBlob](name)

class BlobProperty(name: String) extends ValueProperty[Blob](name)

class PostalAddressProperty(name: String) extends ValueProperty[PostalAddress](name)

class PhoneNumberProperty(name: String) extends ValueProperty[PhoneNumber](name)

class EmailProperty(name: String) extends ValueProperty[Email](name)

class UserProperty(name: String) extends ValueProperty[User](name)

class IMHandleProperty(name: String) extends ValueProperty[IMHandle](name)

class LinkProperty(name: String) extends ValueProperty[Link](name)

class CategoryProperty(name: String) extends ValueProperty[Category](name)

class RatingProperty(name: String) extends ValueProperty[Rating](name)

class BlobKeyProperty(name: String) extends ValueProperty[BlobKey](name)


abstract class StringStoreProperty[P: ClassTag](name: String) extends ValueProperty[P](name) {

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

class StringProperty(name: String) extends StringStoreProperty[String](name) {

  override def fromString(value: String): String = value

  override def toString(value: String): String = value
}

class BigIntProperty(name: String) extends StringStoreProperty[BigInt](name) {

  override def fromString(value: String): BigInt = BigInt(value)

  override def toString(value: BigInt): String = value.toString()
}

class BigDecimalProperty(name: String) extends StringStoreProperty[BigDecimal](name) {

  override def fromString(value: String): BigDecimal = BigDecimal(value)

  override def toString(value: BigDecimal): String = value.toString()
}


class EnumProperty[E <: Enum[E] : ClassTag](name: String) extends StringStoreProperty[E](name) {
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


abstract class ByteProperty[B: ClassTag](name: String) extends ValueProperty[B](name) {

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


class ByteArrayProperty(name: String) extends ByteProperty[Array[Byte]](name) {

  override def fromByte(value: Array[Byte]): Array[Byte] = value

  override def toByte(value: Array[Byte]): Array[Byte] = value
}

class SerializableProperty[E <: Serializable : ClassTag](name: String) extends ByteProperty[E](name) {

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

class JsonProperty[E <: AnyRef : Manifest](name: String) extends StringStoreProperty[E](name) {

  override def fromString(value: String): E = Json.parseAs[E](value)

  override def toString(value: E): String = Json.stringify(value)
}

final class VersionProperty(name: String) extends ValueProperty[Long](name) with IndexedProperty[Long] {

  override def toStoreProperty(value: Long): Any = value + 1
}

final class CreationDateProperty(name: String) extends ValueProperty[Date](name) with IndexedProperty[Date] {

  override def toStoreProperty(value: Date): Any = value match {
    case d: Date => d
    case _ => new Date
  }

}

final class ModificationDateProperty(name: String) extends ValueProperty[Date](name) with IndexedProperty[Date] {
  override def toStoreProperty(value: Date): Any = new Date
}

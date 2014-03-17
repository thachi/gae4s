package com.xhachi.gae4s.datastore

import com.google.appengine.api.datastore.{Key => LLKey}
import com.google.appengine.api.datastore.{Entity => LLEntity}
import com.google.appengine.api.datastore.Query.FilterOperator._
import com.google.appengine.api.datastore.Query.SortDirection._

import java.util.Date

import com.google.appengine.api.datastore._
import com.google.appengine.api.users._
import com.google.appengine.api.blobstore._
import java.io.{ObjectInputStream, ByteArrayInputStream, ObjectOutputStream, ByteArrayOutputStream}
import scala.reflect.ClassTag

object Property {
  val ShortLimit = 500
  val LongLimit = 1000000
}

trait Property[T] {

  def name: String

  def indexed: Boolean = false

  def ==(value: T): Filter = FilterPredicate(name, EQUAL, value)

  def !=(value: T): Filter = FilterPredicate(name, NOT_EQUAL, value)

  def >(value: T): Filter = FilterPredicate(name, GREATER_THAN, value)

  def >=(value: T): Filter = FilterPredicate(name, GREATER_THAN_OR_EQUAL, value)

  def <(value: T): Filter = FilterPredicate(name, LESS_THAN, value)

  def <=(value: T): Filter = FilterPredicate(name, LESS_THAN_OR_EQUAL, value)

  def in(value: T, values: T*): Filter = FilterPredicate(name, IN, value :: values ++: Nil)

  def asc = SortPredicate(name, ASCENDING)

  def desc = SortPredicate(name, DESCENDING)

  def fromStore(implicit entity: LLEntity): T = fromStoreProperty(entity.getProperty(name))

  def toStore(value: T)(implicit entity: LLEntity) = entity.setProperty(name, toStoreProperty(value))

  protected[datastore] def toStoreProperty(value: T): Any

  protected[datastore] def fromStoreProperty(value: Any): T

  protected[datastore] def createFromConversionException(v: Any) = new PropertyConversionException(name + " \""+v+"\"(" + v.getClass.getName + ") can not convert from stored property")

  protected[datastore] def createToConversionException(v: Any) = new PropertyConversionException(name + " \"\"+v+\"\"(\" + v.getClass.getName + \") can not convert to store property")
}

class PropertyConversionException(message: String) extends Exception(message)


trait SimpleProperty[T] extends Property[T] {

  override def toStoreProperty(value: T): Any = value

  override def fromStoreProperty(value: Any): T = value.asInstanceOf[T]
}

case class LongProperty(name: String) extends Property[Long] {
  override def toStoreProperty(value: Long): Any = value

  override def fromStoreProperty(value: Any): Long = value match {
    case v: Long => v
    case v: Double => v.toLong
    case v => throw createFromConversionException(v)
  }
}

case class IntProperty(name: String) extends Property[Int] {
  override def toStoreProperty(value: Int): Any = value

  override def fromStoreProperty(value: Any): Int = value match {
    case v: Long=> v.toInt
    case v: Double => v.toInt
    case v => throw createFromConversionException(v)
  }
}

case class DoubleProperty(name: String) extends SimpleProperty[Double]

case class BooleanProperty(name: String) extends SimpleProperty[Boolean]

case class DateProperty(name: String) extends SimpleProperty[Date]

case class GeoPtProperty(name: String) extends SimpleProperty[GeoPt]

case class ShortBlobProperty(name: String) extends SimpleProperty[ShortBlob]

case class BlobProperty(name: String) extends SimpleProperty[Blob]

case class PostalAddressProperty(name: String) extends SimpleProperty[PostalAddress]

case class PhoneNumberProperty(name: String) extends SimpleProperty[PhoneNumber]

case class EmailProperty(name: String) extends SimpleProperty[Email]

case class UserProperty(name: String) extends SimpleProperty[User]

case class IMHandleProperty(name: String) extends SimpleProperty[IMHandle]

case class LinkProperty(name: String) extends SimpleProperty[Link]

case class CategoryProperty(name: String) extends SimpleProperty[Category]

case class RatingProperty(name: String) extends SimpleProperty[Rating]

@deprecated
case class KeyProperty(name: String) extends SimpleProperty[LLKey]

case class BlobKeyProperty(name: String) extends SimpleProperty[BlobKey]


trait StringStoreProperty[P] extends Property[P] {

  def toString(value: P): String

  def fromString(value: String): P

  final override def toStoreProperty(p: P): Any = {
    val value: String = toString(p)
    val bytes = value.getBytes("UTF-8")
    if (bytes.length < Property.ShortLimit) value
    else if (bytes.length < Property.LongLimit) new Text(value)
    else throw createToConversionException(value)
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

case class StringProperty(name: String) extends StringStoreProperty[String] {

  override def fromString(value: String): String = value

  override def toString(value: String): String = value
}

case class BigIntProperty(name: String) extends StringStoreProperty[BigInt] {

  override def fromString(value: String): BigInt = BigInt(value)

  override def toString(value: BigInt): String = value.toString()
}

case class BigDecimalProperty(name: String) extends StringStoreProperty[BigDecimal] {

  override def fromString(value: String): BigDecimal = BigDecimal(value)

  override def toString(value: BigDecimal): String = value.toString()
}


class EnumProperty[E <: Enum[E] : ClassTag](val name: String) extends StringStoreProperty[E] {
  override def fromString(value: String): E = {
    def enumValueOf[T <: Enum[T]](cls: Class[_], stringValue: String): Enum[_] =
      Enum.valueOf(cls.asInstanceOf[Class[T]], stringValue).asInstanceOf[Enum[_]]
    implicitly[ClassTag[E]].runtimeClass match {
      case clz if classOf[Enum[E]].isAssignableFrom(clz) => enumValueOf(clz, value).asInstanceOf[E]
      case _ => throw new RuntimeException("Enum type error.")
    }
  }

  override def toString(value: E): String = value.name()
}


trait ByteProperty[B] extends Property[B] {

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


case class ByteArrayProperty(name: String) extends ByteProperty[Array[Byte]] {

  override def fromByte(value: Array[Byte]): Array[Byte] = value

  override def toByte(value: Array[Byte]): Array[Byte] = value
}

case class SerializableProperty[E <: Serializable](name: String) extends ByteProperty[E] {

  override def fromByte(value: Array[Byte]): E = {
    val bais = new ByteArrayInputStream(value)
    val ois = new ObjectInputStream(bais)
    ois.readObject().asInstanceOf[E]
  }

  override def toByte(value: E): Array[Byte] = {
    val baos = new ByteArrayOutputStream
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(value)
    oos.close()
    baos.toByteArray
  }
}


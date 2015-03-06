package com.xhachi.gae4s.json

import java.text.SimpleDateFormat

import org.json4s._
import org.json4s.ext.EnumNameSerializer
import org.json4s.native.JsonMethods.{parse => parseByJson4s}
import org.json4s.native.Serialization.write

import scala.collection.mutable

object Json extends Json(None)

/**
 * TODO: もっと汎用的に使えるよう再設計する
 */
class Json(dateFormat: Option[String]) {

  protected val enumSeq = new mutable.HashSet[Enumeration]

  protected val typeHintTargetSeq = new mutable.HashSet[Class[_]]

  implicit protected var _formats: Formats = DefaultFormats

  def formats: Formats = _formats

  def addEnum[E <: Enumeration](enums: E*): Unit = synchronized {

    var unregistered = enums.filter(!enumSeq.contains(_))
    if (0 < unregistered.size) {
      enumSeq ++= unregistered
      buildFormats()
    }
  }

  def addTypeHintTarget(targetClass: Class[_]*): Unit = synchronized {

    var unregistered = targetClass.filter(!typeHintTargetSeq.contains(_))
    if (0 < unregistered.size) {
      typeHintTargetSeq ++= unregistered
      buildFormats()
    }
  }

  private def buildFormats(): Unit = {
    _formats = dateFormat match {
      case Some(f) =>
        new DefaultFormats {
          override def dateFormatter = new SimpleDateFormat(f)
          override val typeHintFieldName: String = "class"
          override val typeHints: TypeHints = FullTypeHints(typeHintTargetSeq.toList)
        } ++ enumSeq.map(e => new EnumNameSerializer(e))
      case None =>
        new DefaultFormats {
          override def dateFormatter = DefaultFormats.losslessDate()
          override val typeHintFieldName: String = "class"
          override val typeHints: TypeHints = FullTypeHints(typeHintTargetSeq.toList)
        } ++ enumSeq.map(e => new EnumNameSerializer(e))
    }
  }
  buildFormats()

  /**
   * Convert JSON String to case class.
   *
   * @param value Instance of case class
   * @tparam E Class of case class
   * @return Converted case class instance
   */
  def parseAs[E: Manifest](value: String): E = parse(value).extract[E]

  /**
   * Convert JValue to case class.
   *
   * @param value Instance of JValue
   * @tparam E Class of case class
   * @return Converted case class instance
   */
  def extractAs[E: Manifest](value: JValue): E = value.extract[E]

  /**
   * Convert JSON String to JValue.
   *
   * @param value String as JSON
   * @return Converted JValue
   */
  def parse(value: String) = parseByJson4s(value)

  /**
   * Convert case class to JSON String.
   *
   * @param value Instance of case class
   * @tparam E Class of case class
   * @return Converted json string
   */
  def stringify[E <: AnyRef : Manifest](value: E): String = write[E](value)

  /**
   * Convert case class to JValue.
   *
   * @param value Instance of case class
   * @tparam E Class of case class
   * @return Converted JValue
   */
  def decompose[E <: AnyRef](value: E): JValue = Extraction.decompose(value)

  /**
   * Convert JValue to JSON String.
   *
   * @param value Instance of JValue
   * @return Converted JSON String
   */
  def stringify(value: JValue): String = write(value)


}

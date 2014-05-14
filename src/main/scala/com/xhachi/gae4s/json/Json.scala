package com.xhachi.gae4s.json

import scala.collection.mutable
import org.json4s.ext.EnumNameSerializer

import org.json4s._
import org.json4s.native.Serialization.write
import org.json4s.native.JsonMethods.{parse => parseByJson4s}

object Json extends Json

class Json {

  private val enumSeq = new mutable.HashSet[Enumeration]

  private val typeHintTargetSeq = new mutable.HashSet[Class[_]]

  implicit private var _formats: Formats = DefaultFormats

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
    _formats = new DefaultFormats {
      override val typeHintFieldName: String = "class"
      override val typeHints: TypeHints = FullTypeHints(typeHintTargetSeq.toList)
    } ++ enumSeq.map(e => new EnumNameSerializer(e))
  }

  def parseAs[E: Manifest](value: String): E = parse(value).extract[E]

  def parse(value: String) = parseByJson4s(value)

  def stringify[E <: AnyRef : Manifest](value: E): String = write[E](value)

  def stringify(value: JValue): String = write(value)
}

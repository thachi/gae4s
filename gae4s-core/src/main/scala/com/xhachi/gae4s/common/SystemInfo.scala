package com.xhachi.gae4s.common

object SystemInfo {

  def propertyOption(key: String): Option[String] = Option(property(key))

  def property(key: String): String = System.getProperty(key)
}

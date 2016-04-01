package com.xhachi.gae4s.common

object SystemInfo {

  def getPropertyOption(key: String): Option[String] = getProperty(key) match {
    case s: String => Some(s)
    case _ => None
  }

  def getProperty(key: String): String = {
    System.getProperty(key)
  }
}

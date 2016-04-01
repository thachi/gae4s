package com.xhachi.gae4s.common

import com.google.appengine.api.utils.SystemProperty

/**
  * AppEngineのアプリケーション情報を取得するためのユーティリティです。
  */
object AppInfo {

  val Runtime = "com.google.appengine.runtime"

  val EnvironmentKey = Runtime + ".environment"

  def version: String = versionOption.getOrElse(throw new IllegalStateException("version is not defined."))

  def versionOption: Option[String] = SystemProperty.applicationVersion.get() match {
    case v: String => Some(v)
    case _ => None
  }

  def id: String = idOption.getOrElse(throw new IllegalStateException("id is not defined."))

  def idOption: Option[String] = SystemProperty.applicationId.get() match {
    case i: String => Some(i)
    case _ => None
  }

  def environment = System.getProperty(EnvironmentKey) match {
    case null => None
    case e => Some(e)
  }

  def isServer: Boolean = environment.isDefined

  def isDevelopment: Boolean = environment.contains("Development")

  def isProduction: Boolean = environment.contains("Production")


}

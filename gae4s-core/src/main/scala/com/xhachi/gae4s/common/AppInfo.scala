package com.xhachi.gae4s.common

import com.google.appengine.api.utils.SystemProperty

/**
  * AppEngineのアプリケーション情報を取得するためのユーティリティです。
  */
object AppInfo {

  private val RuntimePrefix: String = "com.google.appengine.runtime"

  private val EnvironmentKey: String = RuntimePrefix + ".environment"

  def version: String = versionOption.getOrElse(throw new IllegalStateException("version is not defined."))

  def versionOption: Option[String] = Option(SystemProperty.applicationVersion.get)

  def id: String = idOption.getOrElse(throw new IllegalStateException("id is not defined."))

  def idOption: Option[String] = Option(SystemProperty.applicationId.get)

  def environmentOption: Option[String] = Option(System.getProperty(EnvironmentKey))

  def environment: String = environmentOption.getOrElse(throw new IllegalStateException("environment is not defined."))

  def isServer: Boolean = environmentOption.isDefined

  def isDevelopment: Boolean = environmentOption.contains("Development")

  def isProduction: Boolean = environmentOption.contains("Production")


}

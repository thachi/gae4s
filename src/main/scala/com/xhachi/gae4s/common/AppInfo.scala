package com.xhachi.gae4s.common

import com.google.appengine.api.utils.SystemProperty

/**
 * AppEngineのアプリケーション情報を取得するためのユーティリティです。
 */
object AppInfo {

  @deprecated
  def version = SystemProperty.applicationVersion.get()

  def versionOption: Option[String] = SystemProperty.applicationVersion.get() match {
    case v: String => Some(v)
    case _ =>  None
  }

  @deprecated
  def id = SystemProperty.applicationId.get()

  def idOption: Option[String] = SystemProperty.applicationId.get() match {
    case i: String => Some(i)
    case _ =>  None
  }
}

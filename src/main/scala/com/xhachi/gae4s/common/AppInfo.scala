package com.xhachi.gae4s.common

import com.google.appengine.api.utils.SystemProperty

/**
 * AppEngineのアプリケーション情報を取得するためのユーティリティです。
 */
object AppInfo {

  def versionOption: Option[String] = SystemProperty.applicationVersion.get() match {
    case v: String => Some(v)
    case _ =>  None
  }

  def idOption: Option[String] = SystemProperty.applicationId.get() match {
    case i: String => Some(i)
    case _ =>  None
  }
}

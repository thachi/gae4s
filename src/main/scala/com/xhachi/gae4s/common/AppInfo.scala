package com.xhachi.gae4s.common

import com.google.appengine.api.utils.SystemProperty

/**
 * AppEngineのアプリケーション情報を取得するためのユーティリティです。
 */
object AppInfo {

  def version = SystemProperty.applicationVersion.get()

  def id = SystemProperty.applicationId.get()

}

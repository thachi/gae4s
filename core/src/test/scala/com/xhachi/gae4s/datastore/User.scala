package com.xhachi.gae4s.datastore

import com.xhachi.gae4s.datastore.annotations._

@entity
class User(val key: Key[User])
  extends Entity[User]
  with Version
  with CreatedAt
  with UpdatedAt {

  var name: String = ""

  @indexed
  var height: Int = 0
  @indexed
  var weight: Int = 0

  var mobilePhone: Option[String] = None
  var webInfo: WebInfo = WebInfo()

  @indexed
  var deleted: Boolean = false

}

case class WebInfo(email: Option[String] = None, twitter: Option[String] = None)

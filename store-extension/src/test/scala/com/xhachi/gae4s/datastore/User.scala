package com.xhachi.gae4s.datastore

import com.xhachi.gae4s.datastore.annotations._

class User(val key: Key[User], @indexed var name: String = "")
  extends Entity[User]
  with CreatedAt
  with Version
  with UpdatedAt {

  @indexed
  var height: Int = 0

  @indexed
  var weight: Int = 0
  var mobilePhone: Option[String] = None

  @json
  var webInfo = WebInfo()

  @indexed
  var deleted = false
}

case class WebInfo(email: Option[String] = None, twitter: Option[String] = None)


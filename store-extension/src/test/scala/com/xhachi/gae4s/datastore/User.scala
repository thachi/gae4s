package com.xhachi.gae4s.datastore

import com.xhachi.gae4s.datastore.meta.property

class User(val key: Key[User], @property(indexed = true) var name: String = "")
  extends Entity[User]
  with CreatedAt
  with Version
  with UpdatedAt {

  @property(indexed = true)
  var height: Int = 0

  @property(indexed = true)
  var weight: Int = 0

  @property(indexed = true)
  var mobilePhone: Option[String] = None

  @property(json = true)
  var webInfo = WebInfo()

  @property(indexed = true)
  var deleted = false
}

case class WebInfo(email: Option[String] = None, twitter: Option[String] = None)


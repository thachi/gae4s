package com.xhachi.gae4s.datastore

import com.xhachi.gae4s.datastore.annotations._

@entity
class User(val key: Key[User])
  extends Entity[User]
  with CreatedAt
  with Version
  with UpdatedAt {

  def this(key: Key[User], name: String) = {
    this(key)
    this.name = name
  }

  @indexed
  var name: String = ""

  @indexed
  var height: Int = 0

  @indexed
  var weight: Int = 0
  var mobilePhone: Option[String] = None

  var webInfo = WebInfo()

  @indexed
  var deleted = false
}

case class WebInfo(email: Option[String] = None, twitter: Option[String] = None)


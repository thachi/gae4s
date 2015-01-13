package com.xhachi.gae4s.datastore

import com.xhachi.gae4s.datastore.annotations._

@entity
class User(val key: Key[User],
           var name: String = "",
           @indexed var height: Int = 0,
           @indexed var weight: Int = 0,
           var mobilePhone: Option[String] = None,
           var webInfo: WebInfo = WebInfo(),
           @indexed var deleted: Boolean = false,
           @indexed var spouse: Option[Key[User]] = None)
  extends Entity[User]
  with Version
  with CreatedAt
  with UpdatedAt {

  @indexed
  def twitter: Option[String] = webInfo.twitter
}

case class WebInfo(email: Option[String] = None, twitter: Option[String] = None)


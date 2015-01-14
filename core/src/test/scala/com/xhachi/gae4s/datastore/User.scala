package com.xhachi.gae4s.datastore

import java.util.Date

import com.xhachi.gae4s.datastore.annotations._
import scala.concurrent.duration._

class User(val key: Key[User],
           var name: String = "",
           @indexed var height: Int = 0,
           @indexed var weight: Int = 0,
           var mobilePhone: Option[String] = None,
           var webInfo: WebInfo = WebInfo(),
           @indexed var deleted: Boolean = false,
           @indexed var spouse: Option[Key[User]] = None,
           var spouse1: Option[Key[User]] = None,
           @indexed var spouse2: Key[User] = null,
           var spouse3: Key[User] = null)
  extends Entity[User]
  with Version
  with CreatedAt
  with UpdatedAt {

  @indexed
  def twitter: Option[String] = webInfo.twitter
}

case class WebInfo(email: Option[String] = None, twitter: Option[String] = None)

class UserInfo(val key: Key[UserInfo],
               @indexed var role: com.xhachi.gae4s.datastore.UserRoles.Value = UserRoles.Guest,
               var role2: com.xhachi.gae4s.datastore.UserRoles.Value = UserRoles.Guest,
               @indexed var lastLoginDate: Option[Date] = None,
               var lastLoginDevice: Option[String] = None)
  extends Entity[UserInfo]
  with Ancestor[User] {

  @indexed
  def loggedIn = lastLoginDate.isDefined

  @transient
  def durationFromLastLoginDate = lastLoginDate.map(l => new Date().getTime - l.getTime).map(_.milliseconds)
}

object UserRoles extends Enumeration {
  type UserRole = Value

  val Admin, User, Guest = Value
}
package com.xhachi.gae4s.datastore

import java.util.Date

import com.xhachi.gae4s.datastore.meta.property

import scala.concurrent.duration._

class User(val key: Key[User],
           @property var name: String = "",
           @property(indexed = true) var height: Int = 0,
           @property(indexed = true) var weight: Int = 0,
           @property var mobilePhone: Option[String] = None,
           @property(json = true) var webInfo: WebInfo = WebInfo(),
           @property(indexed = true) var deleted: Boolean = false,
           @property(indexed = true) var spouse: Option[Key[User]] = None,
           @property var spouse1: Option[Key[User]] = None,
           @property(indexed = true) var spouse2: Key[User] = null,
           @property var spouse3: Key[User] = null)
  extends Entity[User]
  with Version
  with CreatedAt
  with UpdatedAt {

  @property(indexed = true)
  def twitter: Option[String] = webInfo.twitter
}

case class WebInfo(email: Option[String] = None, twitter: Option[String] = None)

class UserInfo(val key: Key[UserInfo],
               @property(indexed = true)  var role: UserRoles.UserRole = UserRoles.Guest,
               @property var role2: UserRoles.UserRole = UserRoles.Guest,
               @property(indexed = true)  var lastLoginDate: Option[Date] = None,
               @property var lastLoginDevice: Option[String] = None,
               @property var flags: Seq[String] = Nil)
  extends Entity[UserInfo]
  with Ancestor[User] {

  @property(indexed = true)
  def loggedIn = lastLoginDate.isDefined

  @transient
  def durationFromLastLoginDate = durationFromLastLoginDateTo(new Date)

  @property
  def durationFromLastLoginDateTo(now: Date) = lastLoginDate.map(l => now.getTime - l.getTime).map(_.milliseconds)
}

object UserRoles extends Enumeration {
  type UserRole = Value

  val Admin, User, Guest = Value
}
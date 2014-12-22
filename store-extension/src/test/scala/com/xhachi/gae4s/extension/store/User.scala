package com.xhachi.gae4s.extension.store

import com.xhachi.gae4s.datastore._
import com.xhachi.gae4s.extension.store

class User(val key: Key[User],
           var name: String = "",
           var height: Int = 0,
           var weight: Int = 0,
           var mobilePhone: Option[String] = None,
           var webInfo: WebInfo = store.WebInfo(),
           var deleted: Boolean = false)
  extends Entity[User]
  with CreatedAt
  with Version
  with UpdatedAt

case class WebInfo(email: Option[String] = None, twitter: Option[String] = None)


class UserMeta extends EntityMeta[User] with CreatedAtMeta with UpdatedAtMeta with VersionMeta {

  val name = new StringProperty("name") with IndexedProperty[String]
  val height = new IntProperty("height") with IndexedProperty[Int]
  val weight = new IntProperty("weight") with IndexedProperty[Int]
  val mobilePhone = new OptionProperty(new StringProperty("mobilePhone"))
  val webInfo = new SerializableProperty[WebInfo]("webInfo")
  val deleted = new BooleanProperty("deleted") with IndexedProperty[Boolean]

  override def properties = super.properties ++ Seq(name, height, weight, mobilePhone, webInfo, deleted)


  override def createEntity(key: Key[User]) = new User(key)

  addApplyFromLLEntity {
    (from: LLEntity, to: User) =>
      to.name = name.getValueFromLLEntity(from)
      to.height = height.getValueFromLLEntity(from)
      to.weight = weight.getValueFromLLEntity(from)
      to.mobilePhone = mobilePhone.getValueFromLLEntity(from)
      to.webInfo = webInfo.getValueFromLLEntity(from)
      to.deleted = deleted.getValueFromLLEntity(from)
  }

  addApplyToLLEntity {
    (from: User, to: LLEntity) =>
      name.setValueToLLEntity(to)(from.name)
      height.setValueToLLEntity(to)(from.height)
      weight.setValueToLLEntity(to)(from.weight)
      webInfo.setValueToLLEntity(to)(from.webInfo)
      deleted.setValueToLLEntity(to)(from.deleted)
  }
}
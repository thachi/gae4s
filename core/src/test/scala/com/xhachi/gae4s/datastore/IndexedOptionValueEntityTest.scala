package com.xhachi.gae4s.datastore

import java.math.BigInteger
import java.util.Date

import com.google.appengine.api.blobstore.BlobKey
import com.google.appengine.api.datastore._
import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig
import com.xhachi.gae4s.datastore.annotations.indexed
import com.xhachi.gae4s.tests.AppEngineTestSuite
import org.scalatest.FunSuite

class IndexedOptionValueEntityTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  test("IndexedOptionValueEntityのMetaが正しく生成されること") {

    val meta = EntityMeta.createMeta[IndexedOptionValueEntity]

    assert(meta.properties.size == 22)

    for (p <- meta.properties) {
      assert(p.isInstanceOf[IndexedProperty[_]], p.name)
      assert(p.isInstanceOf[OptionProperty[_]], p.name)
    }

    assert(meta.property("userKey").get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[KeyProperty[_]])
    assert(meta.property("string").get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[StringProperty])
    assert(meta.property("int").get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[IntProperty])
    assert(meta.property("long").get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[LongProperty])
    assert(meta.property("double").get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[DoubleProperty])
    assert(meta.property("bool").get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[BooleanProperty])
    assert(meta.property("date").get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[DateProperty])
    assert(meta.property("geoPt").get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[GeoPtProperty])
    assert(meta.property("shortBlob").get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[ShortBlobProperty])
    assert(meta.property("blob").get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[BlobProperty])
    assert(meta.property("postalAddress").get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[PostalAddressProperty])
    assert(meta.property("phoneNumber").get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[PhoneNumberProperty])
    assert(meta.property("email").get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[EmailProperty])
    assert(meta.property("user").get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[UserProperty])
    assert(meta.property("imHandle").get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[IMHandleProperty])
    assert(meta.property("link").get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[LinkProperty])
    assert(meta.property("category").get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[CategoryProperty])
    assert(meta.property("rating").get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[RatingProperty])
    assert(meta.property("blobKey").get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[BlobKeyProperty])

    assert(meta.property("bigInt").get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[BigIntProperty])
    assert(meta.property("bigDecimal").get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[BigDecimalProperty])
    assert(meta.property("javaEnum").get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[StringStoreProperty[_]])
//    assert(meta.property("scalaEnum").get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[StringStoreProperty[_]])
  }

  test("保存して読み込めること") {

    val key = Datastore.allocateKey[IndexedOptionValueEntity]
    val e = new IndexedOptionValueEntity(key)
    e.userKey = Some(Datastore.allocateKey)
    e.string = Some("test_string")
    e.int = Some(1)
    e.long = Some(2)
    e.double = Some(3)
    e.bool = Some(true)
    e.date = Some(new Date(5))
    e.geoPt = Some(new GeoPt(6, 7))
    e.shortBlob = Some(new ShortBlob("8".getBytes("UTF-8")))
    e.blob = Some(new Blob("9".getBytes("UTF-8")))
    e.postalAddress = Some(new PostalAddress("123-4567"))
    e.phoneNumber = Some(new PhoneNumber("0120-123-456"))
    e.email = Some(new Email("test@example.com"))
    e.user = Some(new com.google.appengine.api.users.User("test2@example.com", "example.com"))
    e.imHandle = Some(new IMHandle(IMHandle.Scheme.sip, "test3"))
    e.link = Some(new Link("http://facebook.com"))
    e.category = Some(new Category("test_category"))
    e.rating = Some(new Rating(99))
    e.blobKey = Some(new BlobKey("123"))
    e.bigInt = Some(new BigInt(new BigInteger("12345678")))
    e.bigDecimal = Some(new BigDecimal(new java.math.BigDecimal("12345678")))
    e.javaEnum = Some(JavaEnum.JAVA_ENUM2)
//    e.scalaEnum = Some(ScalaEnum.ScalaEnum2)
    Datastore.put(e)

    val a = Datastore.get(key)
    assert(e.userKey == a.userKey)
    assert(e.string == a.string)
    assert(e.int == a.int)
    assert(e.long == a.long)
    assert(e.double == a.double)
    assert(e.bool == a.bool)
    assert(e.date == a.date)
    assert(e.geoPt == a.geoPt)
    assert(e.shortBlob == a.shortBlob)
    assert(e.blob == a.blob)
    assert(e.postalAddress == a.postalAddress)
    assert(e.phoneNumber == a.phoneNumber)
    assert(e.email == a.email)
    assert(e.user == a.user)
    assert(e.imHandle == a.imHandle)
    assert(e.link == a.link)
    assert(e.category == a.category)
    assert(e.rating == a.rating)
    assert(e.blobKey == a.blobKey)
    assert(e.bigInt == a.bigInt)
    assert(e.bigDecimal == a.bigDecimal)
    assert(e.javaEnum == a.javaEnum)
//    assert(e.scalaEnum == a.scalaEnum)
  }
}

class IndexedOptionValueEntity(val key: Key[IndexedOptionValueEntity]) extends Entity[IndexedOptionValueEntity] {
  @indexed var userKey: Option[Key[User]] = Some(Datastore.allocateKey[User])
  @indexed var string: Option[String] = Some("")
  @indexed var int: Option[Int] = Some(0)
  @indexed var long: Option[Long] = Some(0)
  @indexed var double: Option[Double] = Some(0)
  @indexed var bool: Option[Boolean] = Some(false)
  @indexed var date: Option[Date] = Some(new Date(0))
  @indexed var geoPt: Option[GeoPt] = Some(new GeoPt(0, 0))
  @indexed var shortBlob: Option[ShortBlob] = Some(new ShortBlob("shot_blob".getBytes("UTF-8")))
  @indexed var blob: Option[Blob] = Some(new Blob("blob".getBytes("UTF-8")))
  @indexed var postalAddress: Option[PostalAddress] = Some(new PostalAddress("060-0806"))
  @indexed var phoneNumber: Option[PhoneNumber] = Some(new PhoneNumber("0120-501353"))
  @indexed var email: Option[Email] = Some(new Email("eample@example.com"))
  @indexed var user: Option[com.google.appengine.api.users.User] = Some(new com.google.appengine.api.users.User("sample@example.com", "example.com"))
  @indexed var imHandle: Option[IMHandle] = Some(null)
  @indexed var link: Option[Link] = Some(new Link("http://google.com"))
  @indexed var category: Option[Category] = Some(new Category("category"))
  @indexed var rating: Option[Rating] = Some(new Rating(0))
  @indexed var blobKey: Option[BlobKey] = Some(new BlobKey(""))
  @indexed var bigInt: Option[BigInt] = Some(BigInt(0))
  @indexed var bigDecimal: Option[BigDecimal] = Some(BigDecimal(0))
  @indexed var javaEnum: Option[JavaEnum] = Some(JavaEnum.JAVA_ENUM1)
//  @indexed var scalaEnum: Option[ScalaEnum.Value] = Some(ScalaEnum.ScalaEnum1)
}




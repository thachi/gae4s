package com.xhachi.gae4s.datastore

import java.math
import java.math.BigInteger
import java.util.Date

import com.google.appengine.api.blobstore.BlobKey
import com.google.appengine.api.datastore._
import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig
import com.xhachi.gae4s.tests.AppEngineTestSuite
import org.scalatest.FunSuite

class SeqValueEntityTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  test("SeqValueEntityのMetaが正しく生成されること") {

    val meta = EntityMeta.createMeta[SeqValueEntity]

    assert(meta.properties.size == 22)

    for (p <- meta.properties) {
      assert(!p.isInstanceOf[IndexedProperty[_]], p.name)
      assert(p.isInstanceOf[SeqProperty[_]], p.name)
    }

    assert(meta.property("string").get.asInstanceOf[SeqProperty[_]].property.isInstanceOf[StringProperty])
    assert(meta.property("int").get.asInstanceOf[SeqProperty[_]].property.isInstanceOf[IntProperty])
    assert(meta.property("long").get.asInstanceOf[SeqProperty[_]].property.isInstanceOf[LongProperty])
    assert(meta.property("double").get.asInstanceOf[SeqProperty[_]].property.isInstanceOf[DoubleProperty])
    assert(meta.property("bool").get.asInstanceOf[SeqProperty[_]].property.isInstanceOf[BooleanProperty])
    assert(meta.property("date").get.asInstanceOf[SeqProperty[_]].property.isInstanceOf[DateProperty])
    assert(meta.property("geoPt").get.asInstanceOf[SeqProperty[_]].property.isInstanceOf[GeoPtProperty])
    assert(meta.property("shortBlob").get.asInstanceOf[SeqProperty[_]].property.isInstanceOf[ShortBlobProperty])
    assert(meta.property("blob").get.asInstanceOf[SeqProperty[_]].property.isInstanceOf[BlobProperty])
    assert(meta.property("postalAddress").get.asInstanceOf[SeqProperty[_]].property.isInstanceOf[PostalAddressProperty])
    assert(meta.property("phoneNumber").get.asInstanceOf[SeqProperty[_]].property.isInstanceOf[PhoneNumberProperty])
    assert(meta.property("email").get.asInstanceOf[SeqProperty[_]].property.isInstanceOf[EmailProperty])
    assert(meta.property("user").get.asInstanceOf[SeqProperty[_]].property.isInstanceOf[UserProperty])
    assert(meta.property("imHandle").get.asInstanceOf[SeqProperty[_]].property.isInstanceOf[IMHandleProperty])
    assert(meta.property("link").get.asInstanceOf[SeqProperty[_]].property.isInstanceOf[LinkProperty])
    assert(meta.property("category").get.asInstanceOf[SeqProperty[_]].property.isInstanceOf[CategoryProperty])
    assert(meta.property("rating").get.asInstanceOf[SeqProperty[_]].property.isInstanceOf[RatingProperty])
    assert(meta.property("blobKey").get.asInstanceOf[SeqProperty[_]].property.isInstanceOf[BlobKeyProperty])

    assert(meta.property("bigInt").get.asInstanceOf[SeqProperty[_]].property.isInstanceOf[BigIntProperty])
    assert(meta.property("bigDecimal").get.asInstanceOf[SeqProperty[_]].property.isInstanceOf[BigDecimalProperty])
    assert(meta.property("javaEnum").get.asInstanceOf[SeqProperty[_]].property.isInstanceOf[StringStoreProperty[_]])
    assert(meta.property("scalaEnum").get.asInstanceOf[SeqProperty[_]].property.isInstanceOf[StringStoreProperty[_]])
  }

  test("保存して読み込めること") {

    val key = Datastore.allocateKey[SeqValueEntity]
    val e = new SeqValueEntity(key)
    e.string = Seq("test_string")
    e.int = Seq(1)
    e.long = Seq(2)
    e.double = Seq(3)
    e.bool = Seq(true)
    e.date = Seq(new Date(5))
    e.geoPt = Seq(new GeoPt(6, 7))
    e.shortBlob = Seq(new ShortBlob("8".getBytes("UTF-8")))
    e.blob = Seq(new Blob("9".getBytes("UTF-8")))
    e.postalAddress = Seq(new PostalAddress("123-4567"))
    e.phoneNumber = Seq(new PhoneNumber("0120-123-456"))
    e.email = Seq(new Email("test@example.com"))
    e.user = Seq(new com.google.appengine.api.users.User("test2@example.com", "example.com"))
    e.imHandle = Seq(new IMHandle(IMHandle.Scheme.sip, "test3"))
    e.link = Seq(new Link("http://facebook.com"))
    e.category = Seq(new Category("test_category"))
    e.rating = Seq(new Rating(99))
    e.blobKey = Seq(new BlobKey("123"))
    e.bigInt = Seq(new BigInt(new BigInteger("12345678")))
    e.bigDecimal = Seq(new BigDecimal(new math.BigDecimal("12345678")))
    e.javaEnum = Seq(JavaEnum.JAVA_ENUM2)
    e.scalaEnum = Seq(ScalaEnum.ScalaEnum2)
    Datastore.put(e)

    val a = Datastore.get(key)
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
    assert(e.scalaEnum == a.scalaEnum)

  }
}

class SeqValueEntity(val key: Key[SeqValueEntity]) extends Entity[SeqValueEntity] {
  var string: Seq[String] = Seq("")
  var int: Seq[Int] = Seq(0)
  var long: Seq[Long] = Seq(0)
  var double: Seq[Double] = Seq(0)
  var bool: Seq[Boolean] = Seq(false)
  var date: Seq[Date] = Seq(new Date(0))
  var geoPt: Seq[GeoPt] = Seq(new GeoPt(0, 0))
  var shortBlob: Seq[ShortBlob] = Seq(new ShortBlob("shot_blob".getBytes("UTF-8")))
  var blob: Seq[Blob] = Seq(new Blob("blob".getBytes("UTF-8")))
  var postalAddress: Seq[PostalAddress] = Seq(new PostalAddress("060-0806"))
  var phoneNumber: Seq[PhoneNumber] = Seq(new PhoneNumber("0120-501353"))
  var email: Seq[Email] = Seq(new Email("eample@example.com"))
  var user: Seq[com.google.appengine.api.users.User] = Seq(new com.google.appengine.api.users.User("sample@example.com", "example.com"))
  var imHandle: Seq[IMHandle] = Seq(null)
  var link: Seq[Link] = Seq(new Link("http://google.com"))
  var category: Seq[Category] = Seq(new Category("category"))
  var rating: Seq[Rating] = Seq(new Rating(0))
  var blobKey: Seq[BlobKey] = Seq(new BlobKey(""))
  var bigInt: Seq[BigInt] = Seq(BigInt(0))
  var bigDecimal: Seq[BigDecimal] = Seq(BigDecimal(0))
  var javaEnum: Seq[JavaEnum] = Seq(JavaEnum.JAVA_ENUM1)
  var scalaEnum: Seq[ScalaEnum.Value] = Seq(ScalaEnum.ScalaEnum1)
}






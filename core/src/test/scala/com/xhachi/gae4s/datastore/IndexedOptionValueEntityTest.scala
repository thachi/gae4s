package com.xhachi.gae4s.datastore

import java.math.BigInteger
import java.util.Date

import com.google.appengine.api.blobstore.BlobKey
import com.google.appengine.api.datastore._
import com.google.appengine.api.users
import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig
import com.xhachi.gae4s.datastore.meta.property
import com.xhachi.gae4s.tests.AppEngineTestSuite
import org.scalatest.FunSuite

class IndexedOptionValueEntity1Test extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  test("IndexedOptionValueEntity1のMetaが正しく生成されること") {

    val meta = EntityMeta.createMeta[IndexedOptionValueEntity1]

    assert(meta.properties.size == 11)
    assert(meta.property("userKey").get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[KeyProperty[_]])

    def assertProperty(name: String, propertyType: Class[_]) = {
      assert(meta.property(name).isDefined, name)
      assert(meta.property(name).get.isInstanceOf[IndexedProperty[_]], name)
      assert(meta.property(name).get.isInstanceOf[OptionProperty[_]], name)
      assert(meta.property(name).get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[ValueProperty[_]], name)
      assert(meta.property(name).get.asInstanceOf[OptionProperty[_]].property.asInstanceOf[ValueProperty[_]].propertyType == propertyType, name)
    }

    assertProperty("string", classOf[String])
    assertProperty("int", classOf[Int])
    assertProperty("long", classOf[Long])
    assertProperty("double", classOf[Double])
    assertProperty("bool", classOf[Boolean])
    assertProperty("date", classOf[Date])
    assertProperty("geoPt", classOf[GeoPt])
    assertProperty("text", classOf[Text])
    assertProperty("shortBlob", classOf[ShortBlob])
    assertProperty("blob", classOf[Blob])
  }

  test("保存して読み込めること") {

    val key = Datastore.allocateKey[IndexedOptionValueEntity1]
    val e = new IndexedOptionValueEntity1(key)
    e.userKey = Some(Datastore.allocateKey)
    e.string = Some("test_string")
    e.int = Some(1)
    e.long = Some(2)
    e.double = Some(3)
    e.bool = Some(true)
    e.date = Some(new Date(5))
    e.geoPt = Some(new GeoPt(6, 7))
    e.text = Some(new Text("text7"))
    e.shortBlob = Some(new ShortBlob("8".getBytes("UTF-8")))
    e.blob = Some(new Blob("9".getBytes("UTF-8")))
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
    assert(e.text == a.text)
    assert(e.shortBlob == a.shortBlob)
    assert(e.blob == a.blob)
  }

  test("クエリできること") {
    Datastore.count(Datastore.query[IndexedOptionValueEntity1].filter(_.int == None))

    //FIXME: コンパイル時にNullPointerExceptionが出る。
    //    Datastore.query[IndexedOptionValueEntity].filter(_.scalaEnum == Some(ScalaEnum.ScalaEnum1)).count
  }
}
class IndexedOptionValueEntity2Test extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  test("IndexedOptionValueEntity2のMetaが正しく生成されること") {

    val meta = EntityMeta.createMeta[IndexedOptionValueEntity2]

    assert(meta.properties.size == 13)

    def assertProperty(name: String, propertyType: Class[_]) = {
      assert(meta.property(name).isDefined)
      assert(meta.property(name).get.isInstanceOf[IndexedProperty[_]])
      assert(meta.property(name).get.isInstanceOf[OptionProperty[_]])
      assert(meta.property(name).get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[ValueProperty[_]])
      assert(meta.property(name).get.asInstanceOf[OptionProperty[_]].property.asInstanceOf[ValueProperty[_]].propertyType == propertyType)
    }

    assertProperty("postalAddress", classOf[PostalAddress])
    assertProperty("phoneNumber", classOf[PhoneNumber])
    assertProperty("email", classOf[Email])
    assertProperty("user", classOf[users.User])
    assertProperty("imHandle", classOf[IMHandle])
    assertProperty("link", classOf[Link])
    assertProperty("category", classOf[Category])
    assertProperty("rating", classOf[Rating])
    assertProperty("blobKey", classOf[BlobKey])

    assertProperty("bigInt", classOf[BigInt])
    assertProperty("bigDecimal", classOf[BigDecimal])

    assert(meta.property("javaEnum").get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[StringStoreProperty[_]])
    assert(meta.property("scalaEnum").get.asInstanceOf[OptionProperty[_]].property.isInstanceOf[StringStoreProperty[_]])
  }

  test("保存して読み込めること") {

    val key = Datastore.allocateKey[IndexedOptionValueEntity2]
    val e = new IndexedOptionValueEntity2(key)
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
    e.scalaEnum = Some(ScalaEnum.ScalaEnum2)
    Datastore.put(e)

    val a = Datastore.get(key)
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

  test("クエリできること") {
    Datastore.count(Datastore.query[IndexedOptionValueEntity2].filter(_.scalaEnum == None))

    //FIXME: コンパイル時にNullPointerExceptionが出る。
    //    Datastore.query[IndexedOptionValueEntity].filter(_.scalaEnum == Some(ScalaEnum.ScalaEnum1)).count
  }
}

// TODO: @entity(indexed = true)で全プロパティがindex対象になったほうがいいかも
class IndexedOptionValueEntity1(val key: Key[IndexedOptionValueEntity1]) extends Entity[IndexedOptionValueEntity1] {
  @property(indexed = true) var userKey: Option[Key[User]] = Some(Datastore.allocateKey[User])
  @property(indexed = true) var string: Option[String] = Some("")
  @property(indexed = true) var int: Option[Int] = Some(0)
  @property(indexed = true) var long: Option[Long] = Some(0)
  @property(indexed = true) var double: Option[Double] = Some(0)
  @property(indexed = true) var bool: Option[Boolean] = Some(false)
  @property(indexed = true) var date: Option[Date] = Some(new Date(0))
  @property(indexed = true) var geoPt: Option[GeoPt] = Some(new GeoPt(0, 0))
  @property(indexed = true) var text: Option[Text] = Some(new Text(null)) //FIXME:インデックスは張れないのでエラーにすべき
  @property(indexed = true) var shortBlob: Option[ShortBlob] = Some(new ShortBlob("shot_blob".getBytes("UTF-8")))
  @property(indexed = true) var blob: Option[Blob] = Some(new Blob("blob".getBytes("UTF-8")))
}

class IndexedOptionValueEntity2(val key: Key[IndexedOptionValueEntity2]) extends Entity[IndexedOptionValueEntity2] {
  @property(indexed = true) var postalAddress: Option[PostalAddress] = Some(new PostalAddress("060-0806"))
  @property(indexed = true) var phoneNumber: Option[PhoneNumber] = Some(new PhoneNumber("0120-501353"))
  @property(indexed = true) var email: Option[Email] = Some(new Email("eample@example.com"))
  @property(indexed = true) var user: Option[com.google.appengine.api.users.User] = Some(new com.google.appengine.api.users.User("sample@example.com", "example.com"))
  @property(indexed = true) var imHandle: Option[IMHandle] = Some(null)
  @property(indexed = true) var link: Option[Link] = Some(new Link("http://google.com"))
  @property(indexed = true) var category: Option[Category] = Some(new Category("category"))
  @property(indexed = true) var rating: Option[Rating] = Some(new Rating(0))
  @property(indexed = true) var blobKey: Option[BlobKey] = Some(new BlobKey(""))
  @property(indexed = true) var bigInt: Option[BigInt] = Some(BigInt(0))
  @property(indexed = true) var bigDecimal: Option[BigDecimal] = Some(BigDecimal(0))
  @property(indexed = true) var javaEnum: Option[JavaEnum] = Some(JavaEnum.JAVA_ENUM1)
  @property(indexed = true) var scalaEnum: Option[ScalaEnum.Value] = Some(ScalaEnum.ScalaEnum1)
}




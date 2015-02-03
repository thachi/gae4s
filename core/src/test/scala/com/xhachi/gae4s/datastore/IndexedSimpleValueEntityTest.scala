package com.xhachi.gae4s.datastore

import java.math
import java.math.BigInteger
import java.util.Date

import com.google.appengine.api.blobstore.BlobKey
import com.google.appengine.api.datastore._
import com.google.appengine.api.users
import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig
import com.xhachi.gae4s.datastore.meta.property
import com.xhachi.gae4s.tests.AppEngineTestSuite
import org.scalatest.FunSuite

class IndexedSimpleValueEntityTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  test("IndexedSimpleValueEntityのMetaが正しく生成されること") {

    val meta = EntityMeta.createMeta[IndexedSimpleValueEntity]

    assert(meta.properties.size == 24)

    for (p <- meta.properties) {
      assert(p.isInstanceOf[IndexedProperty[_]], p.name)
      assert(!p.isInstanceOf[OptionProperty[_]], p.name)
    }

    def assertProperty(name: String, propertyType: Class[_]) = {
      assert(meta.property(name).isDefined)
      assert(meta.property(name).get.isInstanceOf[ValueProperty[_]])
      assert(meta.property(name).get.asInstanceOf[ValueProperty[_]].propertyType == propertyType)
    }

    assert(meta.property("userKey").get.isInstanceOf[KeyProperty[_]])
    assertProperty("string", classOf[String])
    assertProperty("int", classOf[Int])
    assertProperty("long", classOf[Long])
    assertProperty("double", classOf[Double])
    assertProperty("bool", classOf[Boolean])
    assertProperty("date", classOf[Date])
    assertProperty("geoPt", classOf[GeoPt])
    assertProperty("shortBlob", classOf[ShortBlob])
    assertProperty("blob", classOf[Blob])
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

    assert(meta.property("javaEnum").get.isInstanceOf[EnumProperty[_]])
    assert(meta.property("scalaEnum").get.isInstanceOf[StringStoreProperty[_]])
    assert(meta.property("byteArray").get.isInstanceOf[ByteArrayProperty])
  }

  test("保存して読み込めて検索できること") {

    val key = Datastore.allocateKey[IndexedSimpleValueEntity]
    val key2 = Datastore.allocateKey[IndexedSimpleValueEntity]

    val e = new IndexedSimpleValueEntity(key)
    e.userKey = Datastore.allocateKey[User]
    e.string = "test_string"
    e.int = 1
    e.long = 2
    e.double = 3
    e.bool = true
    e.date = new Date(5)
    e.geoPt = new GeoPt(6, 7)
    e.shortBlob = new ShortBlob("8".getBytes("UTF-8"))
    e.blob = new Blob("9".getBytes("UTF-8"))
    e.postalAddress = new PostalAddress("123-4567")
    e.phoneNumber = new PhoneNumber("0120-123-456")
    e.email = new Email("test@example.com")
    e.user = new com.google.appengine.api.users.User("test2@example.com", "example.com")
    e.imHandle = new IMHandle(IMHandle.Scheme.sip, "test3")
    e.link = new Link("http://facebook.com")
    e.category = new Category("test_category")
    e.rating = new Rating(99)
    e.blobKey = new BlobKey("123")
    e.bigInt = new BigInt(new BigInteger("12345678"))
    e.bigDecimal = new BigDecimal(new math.BigDecimal("12345678"))
    e.javaEnum = JavaEnum.JAVA_ENUM2
    e.scalaEnum = ScalaEnum.ScalaEnum2
    e.byteArray = "test_byte_array".getBytes("UTF-8")
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
    assert(e.scalaEnum == a.scalaEnum)
    assert(e.byteArray.zip(a.byteArray).filterNot(b => b._1 == b._2).isEmpty)

    val q = Datastore.query[IndexedSimpleValueEntity]
    assert(q.filter(_.key == key).asSingleOption.nonEmpty)
    assert(q.filter(_.key == key2).asSingleOption.isEmpty)
    assert(q.filter(_.string == "test_string").asSingleOption.nonEmpty)
    assert(q.filter(_.string == "test_string2").asSingleOption.isEmpty)
    assert(q.filter(_.int == 1).asSingleOption.nonEmpty)
    assert(q.filter(_.int == 2).asSingleOption.isEmpty)
    assert(q.filter(_.long == 2).asSingleOption.nonEmpty)
    assert(q.filter(_.long == 3).asSingleOption.isEmpty)
    assert(q.filter(_.double == 3).asSingleOption.nonEmpty)
    assert(q.filter(_.double == 4).asSingleOption.isEmpty)
    assert(q.filter(_.bool == true).asSingleOption.nonEmpty)
    assert(q.filter(_.bool == false).asSingleOption.isEmpty)

    // FIXME: NOEが出る。
    // assert(q.filter(_.date == new Date(5)).asSingleOption.nonEmpty)
    // assert(q.filter(_.date == new Date(6)).asSingleOption.isEmpty)
    // assert(q.filter(_.geoPt == new GeoPt(6, 7)).asSingleOption.nonEmpty)
    // assert(q.filter(_.geoPt == new GeoPt(6, 8)).asSingleOption.isEmpty)
    // assert(q.filter(_.email == new Email("test@example.com")).asSingleOption.nonEmpty)
    // assert(q.filter(_.email == new Email("test@hoge.example.com")).asSingleOption.isEmpty)

  }


  test("クエリできること") {
    Datastore.query[IndexedSimpleValueEntity].filter(_.scalaEnum == null).count
    Datastore.query[IndexedSimpleValueEntity].filter(_.scalaEnum == ScalaEnum.ScalaEnum1).count
  }

}

class IndexedSimpleValueEntity(val key: Key[IndexedSimpleValueEntity]) extends Entity[IndexedSimpleValueEntity] {
  @property(indexed = true) var userKey: Key[User] = Datastore.allocateKey[User]
  @property(indexed = true) var string: String = ""
  @property(indexed = true) var int: Int = 0
  @property(indexed = true) var long: Long = 0
  @property(indexed = true) var double: Double = 0
  @property(indexed = true) var bool: Boolean = false
  @property(indexed = true) var date: Date = new Date(0)
  @property(indexed = true) var geoPt: GeoPt = new GeoPt(0, 0)
  @property(indexed = true) var shortBlob: ShortBlob = new ShortBlob("shot_blob".getBytes("UTF-8"))
  @property(indexed = true) var blob: Blob = new Blob("blob".getBytes("UTF-8"))
  @property(indexed = true) var postalAddress: PostalAddress = new PostalAddress("060-0806")
  @property(indexed = true) var phoneNumber: PhoneNumber = new PhoneNumber("0120-501353")
  @property(indexed = true) var email: Email = new Email("eample@example.com")
  @property(indexed = true) var user: com.google.appengine.api.users.User = new com.google.appengine.api.users.User("sample@example.com", "example.com")
  @property(indexed = true) var imHandle: IMHandle = null
  @property(indexed = true) var link: Link = new Link("http://google.com")
  @property(indexed = true) var category: Category = new Category("category")
  @property(indexed = true) var rating: Rating = new Rating(0)
  @property(indexed = true) var blobKey: BlobKey = new BlobKey("")
  @property(indexed = true) var bigInt: BigInt = BigInt(0)
  @property(indexed = true) var bigDecimal: BigDecimal = BigDecimal(0)
  @property(indexed = true) var javaEnum: JavaEnum = JavaEnum.JAVA_ENUM1
  @property(indexed = true) var scalaEnum: ScalaEnum.Value = ScalaEnum.ScalaEnum1
  @property(indexed = true) var byteArray: Array[Byte] = "byte_array".getBytes("UTF-8")

}

package com.xhachi.gae4s.datastore

import java.util.Date

import com.google.appengine.api.blobstore.BlobKey
import com.google.appengine.api.datastore._
import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig
import com.xhachi.gae4s.datastore.annotations.{serialize, json}
import com.xhachi.gae4s.tests.AppEngineTestSuite
import org.scalatest.FunSuite

class EntityMacro2Test extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  test("UserのMetaが生成されること") {


    implicit val meta = EntityMeta.createMeta[SimpleValueEntity]

    assert(meta.properties.size == 25)

    for (p <- meta.properties) {
      assert(!p.isInstanceOf[IndexedProperty[_]], p.name)
      assert(!p.isInstanceOf[OptionProperty[_]], p.name)
    }

    assert(meta.property("string").get.isInstanceOf[StringProperty])
    assert(meta.property("int").get.isInstanceOf[IntProperty])
    assert(meta.property("long").get.isInstanceOf[LongProperty])
    assert(meta.property("double").get.isInstanceOf[DoubleProperty])
    assert(meta.property("bool").get.isInstanceOf[BooleanProperty])
    assert(meta.property("date").get.isInstanceOf[DateProperty])
    assert(meta.property("geoPt").get.isInstanceOf[GeoPtProperty])
    assert(meta.property("shortBlob").get.isInstanceOf[ShortBlobProperty])
    assert(meta.property("blob").get.isInstanceOf[BlobProperty])
    assert(meta.property("postalAddress").get.isInstanceOf[PostalAddressProperty])
    assert(meta.property("phoneNumber").get.isInstanceOf[PhoneNumberProperty])
    assert(meta.property("email").get.isInstanceOf[EmailProperty])
    assert(meta.property("user").get.isInstanceOf[UserProperty])
    assert(meta.property("imHandle").get.isInstanceOf[IMHandleProperty])
    assert(meta.property("link").get.isInstanceOf[LinkProperty])
    assert(meta.property("category").get.isInstanceOf[CategoryProperty])
    assert(meta.property("rating").get.isInstanceOf[RatingProperty])
    assert(meta.property("blobKey").get.isInstanceOf[BlobKeyProperty])

    assert(meta.property("bigInt").get.isInstanceOf[BigIntProperty])
    assert(meta.property("bigDecimal").get.isInstanceOf[BigDecimalProperty])
    assert(meta.property("javaEnum").get.isInstanceOf[StringStoreProperty[_]])
    assert(meta.property("scalaEnum").get.isInstanceOf[StringStoreProperty[_]])
    assert(meta.property("byteArray").get.isInstanceOf[ByteArrayProperty])
    assert(meta.property("json").get.isInstanceOf[JsonProperty[_]])
    assert(meta.property("json").get.propertyType == classOf[JsonValue])
    assert(meta.property("serializable").get.isInstanceOf[SerializableProperty[_]])
    assert(meta.property("serializable").get.propertyType == classOf[SerializableValue])
  }
}

class SimpleValueEntity(val key: Key[SimpleValueEntity])
  extends Entity[SimpleValueEntity] {

  var string: String = ""
  var int: Int = 0
  var long: Long = 0
  var double: Double = 0
  var bool: Boolean = false
  var date: Date = new Date(0)
  var geoPt: GeoPt = new GeoPt(0, 0)
  var shortBlob: ShortBlob = new ShortBlob("shot_blob".getBytes("UTF-8"))
  var blob: Blob = new Blob("shot_blob".getBytes("UTF-8"))
  var postalAddress: PostalAddress = new PostalAddress("060-0806")
  var phoneNumber: PhoneNumber = new PhoneNumber("0120-501353")
  var email: Email = new Email("0120-501353")
  var user: com.google.appengine.api.users.User = new com.google.appengine.api.users.User("sample@example.com", "example.com")
  var imHandle: IMHandle = null
  var link: Link = new Link("http://google.com")
  var category: Category = new Category("http://google.com")
  var rating: Rating = new Rating(0)
  var blobKey: BlobKey = new BlobKey("")

  var bigInt: BigInt = BigInt(0)
  var bigDecimal: BigDecimal = BigDecimal(0)
  var javaEnum: JavaEnum = JavaEnum.JAVA_ENUM1
  var scalaEnum: ScalaEnum.Value = ScalaEnum.ScalaEnum1
  var byteArray: Array[Byte] = "byte_array".getBytes("UTF-8")
  @json var json: JsonValue = new JsonValue("test")
  @serialize var serializable: SerializableValue = new SerializableValue("")

}

class SerializableValue(name: String) extends Serializable

class JsonValue(name: String)






package com.xhachi.gae4s.cloudstrage

import java.util.Date

import com.google.appengine.tools.cloudstorage.{GcsServiceFactory, RetryParams}
import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig
import com.xhachi.gae4s.tests.AppEngineTestSuite
import org.json4s.JsonAST.{JString, JValue}
import org.scalatest.FunSuite

class CloudStorageTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  val target: CloudStorage = new CloudStorage(GcsServiceFactory.createGcsService(RetryParams.getDefaultInstance), "test")

  test("とりあえず動かしてみる") {
    val s = "test"
    target.writeBytes("/path/using", s.getBytes)
    val actual = target.readBytes("/path/using")

    assert(actual.isDefined)
    assert(new String(actual.get, "UTF-8") == s)
  }

  test("ファイルがない場合") {
    assert(target.readBytes("/path/using") == None)
  }

  test("ファイルがある場合のmetadata") {
    val now = new Date
    Thread.sleep(1)


    val s = "test"
    target.writeBytes("/path/metadata", s.getBytes)
    val m = target.metadata("/path/metadata")


    assert(m.isDefined)
    //    assert(m.get.getEtag == "")
    assert(m.get.getLastModified.getTime > now.getTime)
    assert(m.get.getLength == 4)
    assert(m.get.getFilename.getBucketName == "test")
    assert(m.get.getFilename.getObjectName == "/path/metadata")
  }

  test("ファイルがない場合のmetadata") {
    assert(target.metadata("/path/using").isEmpty)
  }

  test("writeJson") {
    import org.json4s.JsonDSL._

    val json: JValue = ("name" -> "ロト") ~ ("age" -> 16)

    target.writeJson("/path/json", json)
    val actual = target.readJson("/path/json")

    assert(actual.isDefined)

    val JString(name) = actual.get \ "name"
    assert(name == "ロト")
  }

  test("writeJson(拡張子あり)") {
    import org.json4s.JsonDSL._

    val json: JValue = ("name" -> "ロト") ~ ("age" -> 16)

    target.writeJson("/path/json.json", json)
    val actual = target.readJson("/path/json.json")

    assert(actual.isDefined)

    val JString(name) = actual.get \ "name"
    assert(name == "ロト")
  }


}

package com.xhachi.gae4s.urlfetch

import com.google.appengine.tools.development.testing.LocalURLFetchServiceTestConfig
import com.xhachi.gae4s.tests.AppEngineTestSuite
import org.scalatest.FunSuite

class UrlFetchTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalURLFetchServiceTestConfig :: super.getConfig

  test("GETリクエストができること") {
    val result = UrlFetch.get("http://www.google.com")
    assert(result.status == 200)
    assert(result.content.isDefined)
  }

  test("URLがビルドができること") {
    val result = UrlFetch.buildUrl("http://www.google.com")
    assert(result == "http://www.google.com")
  }

  test("クエリ付きのURLがビルドができること") {
    val result = UrlFetch.buildUrl("http://www.google.com", Map("keyword" -> Seq("*", "=", "あ", "abc")))
    assert(result == "http://www.google.com?keyword=*&keyword=%3D&keyword=%E3%81%82&keyword=abc")
  }


}

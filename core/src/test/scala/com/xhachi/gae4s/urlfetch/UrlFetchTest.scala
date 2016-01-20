package com.xhachi.gae4s.urlfetch

import com.xhachi.gae4s.common.Logger
import com.xhachi.gae4s.tests.{AppEngineTestSuite, LocalServiceTestConfig}
import org.scalatest.FunSuite

class UrlFetchTest extends FunSuite with AppEngineTestSuite with LocalServiceTestConfig.URLFetch {

  test("GETリクエストができること") {
    val result = UrlFetch.get("http://www.google.com")
    assert(result.status == 200)
    assert(result.content.isDefined)
  }

  test("GETリクエストが非同期実行できること") {
    val f = UrlFetch.getAsync("http://www.google.com")
    Logger.info("request begin")
    val result = f.get
    Logger.info("request end")

    assert(result.status == 200)
    assert(result.content.isDefined)
  }

  test("GETリクエストが並列に非同期実行できること") {
    val requests = (1 to 100).map { i =>
      UrlFetch.getAsync("http://www.google.com?q=" + i)
    }
    Logger.info("request begin")
    val responses = requests.map(_.get)
    Logger.info("request end")

    responses.foreach { result =>
      assert(result.status == 200)
      assert(result.content.isDefined)
    }
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

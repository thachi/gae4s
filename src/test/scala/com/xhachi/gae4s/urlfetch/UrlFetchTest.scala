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
}

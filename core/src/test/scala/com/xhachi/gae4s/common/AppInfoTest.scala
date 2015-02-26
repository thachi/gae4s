package com.xhachi.gae4s.common

import org.scalatest.FunSuite

class AppInfoTest extends FunSuite {

  test("AppInfoにアクセスできること") {
    assert(AppInfo.idOption == None)
    assert(AppInfo.versionOption == None)
    assert(!AppInfo.isServer)
    assert(AppInfo.environment == None)
    assert(!AppInfo.isProduction)
    assert(!AppInfo.isDevelopment)
  }

}
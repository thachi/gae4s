package com.xhachi.gae4s

import org.scalatest.FunSuite

class BuildInfoTest extends FunSuite {

  test("BuildInfoにアクセスできること") {
    assert(BuildInfo.name == "gae4s")
    assert(BuildInfo.version != null)
  }

}
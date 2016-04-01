package com.xhachi.gae4s.buildinfo

import org.scalatest.FunSuite

class BuildInfoTest extends FunSuite {

  test("BuildInfoにアクセスできること") {
    assert(BuildInfo.name == "gae4s-core")
    assert(BuildInfo.version != null)
  }

}
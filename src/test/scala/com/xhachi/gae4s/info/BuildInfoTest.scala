package com.xhachi.gae4s.info

import org.scalatest.FunSuite

class BuildInfoTest extends FunSuite {

  test("BuildInfoにアクセスできること") {
    assert(BuildInfo.name == "gae4s")
    assert(BuildInfo.version != null)
  }

}
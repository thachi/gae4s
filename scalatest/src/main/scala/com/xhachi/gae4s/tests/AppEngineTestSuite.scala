package com.xhachi.gae4s.tests

import com.google.appengine.tools.development.testing.{LocalServiceTestConfig, LocalServiceTestHelper}
import org.scalatest.{BeforeAndAfterEach, Suite}

import scala.language.implicitConversions

trait AppEngineTestSuite extends BeforeAndAfterEach {
  self: Suite =>

  //  val logger = Logger.getLogger("com.google")
  //  logger.addHandler(new StreamHandler() {
  //    {
  //      setOutputStream(System.out)
  //      setLevel(Level.OFF)
  //    }
  //  })

  lazy val helper = new LocalServiceTestHelper(getConfig: _*)

  def getConfig: List[LocalServiceTestConfig] = Nil

  override def beforeEach() = helper.setUp()

  override def afterEach() = helper.tearDown()

  //  implicit def keyToContext(key: Key) = new EntityStoreContext {
  //    override def ancestor: Option[Key] = Some(key)
  //  }

}
